
# import concepts ----

folder_path <- here::here("Cohorts", "ConceptSets")

codelist <- omopgenerics::importCodelist(path = folder_path, type = "csv")

# TO CONSIDER TO CHANGE ALL THE CODELIST NAMES HERE
# OR AT THE FILE LEVEL

codelist_treatment <- list(
  "EBRT" = codelist$`[OPTIMA RQ4] EBRT`,
  "Radical prostatectomy" = codelist$`[OPTIMA RQ4] Radical prostatectomy`
)

codelist_stage <- list(
  "cT1-T2" = codelist$`[OPTIMA PCa RQ4] cT1-T2` ,
  "Stage I-II" = codelist$`[OPTIMA PCa RQ4] Stage I-II`
)

codelist$`[OPTIMA PCa RQ4] PSA` <- cdm$concept |>
  dplyr::filter(concept_id %in% codelist$`[OPTIMA PCa RQ4] PSA`) |>
  dplyr::filter(!grepl("free", tolower(.data$concept_name))) |>
  dplyr::pull("concept_id")

# concept cohorts ----

cdm$prostate_cancer_age_50_69 <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = list("prostate_cancer_conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
  name = "prostate_cancer_age_50_69",
  exit = "event_start_date"
) |>
  # SHOULD WE RESTRICT TO ONLY THE FIRST RECORD?
  # require is first record
  # CohortConstructor::requireIsFirstEntry() |>
  # require demographics
  CohortConstructor::requireDemographics(
    sex = "Male",
    ageRange = list(c(50, 69)),
    minPriorObservation = 365, # WHY?
    indexDate = "cohort_start_date"
  )

# cdm$prostate_cancer_first_diagnosis <- CohortConstructor::conceptCohort(cdm,
#                                                                                conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
#                                                                                name = "prostate_cancer_first_diagnosis" ) |>
#   CohortConstructor::requireIsFirstEntry()


# cdm$psa_cohort <- CohortConstructor::measurementCohort(cdm,
#                                                        conceptSet = list("PSA" = codelist$`[OPTIMA PCa RQ4] PSA`),
#                                                        name = "psa_cohort",
#                                                        valueAsNumber = list(c(3, 19.99)) )

cdm$psa_values <- CohortConstructor::measurementCohort(
  cdm = cdm,
  conceptSet = list("psa" = codelist$`[OPTIMA PCa RQ4] PSA`),
  name = "psa_values",
  valueAsNumber = list(c(-Inf, Inf))
) |>
  PatientProfiles::addConceptIntersectField(
    conceptSet = codelist["[OPTIMA PCa RQ4] PSA"],
    window = c(0, 0),
    field = c("value_as_number", "unit_source_value"),
    nameStyle = "{field}"
  ) |>
  # dplyr::left_join(
  #   cdm$measurement |>
  #     dplyr::filter(measurement_concept_id %in% codelist$`[OPTIMA PCa RQ4] PSA`) |>
  #     dplyr::select(person_id, measurement_concept_id, measurement_date, value_as_number, unit_source_value),
  #   by = c("subject_id" = "person_id", "cohort_start_date" = "measurement_date")
  # ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("tr" = c(codelist$`[OPTIMA RQ4] EBRT`, codelist$`[OPTIMA RQ4] Radical prostatectomy`)),
    window = c(0, 180),
    intersections = c(1, Inf)
  ) |>
  CohortConstructor::requireIsLastEntry()

cdm$psa_trial <- cdm$psa_values |>
  dplyr::filter(.data$value_as_number <= 19.99 & .data$value_as_number >=3) |>
  dplyr::compute(name = "psa_trial")

cdm$stage <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = list("c_t1_t2" = codelist$`[OPTIMA PCa RQ4] cT1-T2`),
  name = "stage",
  exit = "event_start_date"
) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("mo" = codelist$`[OPTIMA PCa RQ4] M0`),
    window = c(-Inf, Inf),
    cohortId = "c_t1_t2",
    intersections = c(1, Inf)
  )

# tte ----

# create base cohort
cdm$optima_pc_trial <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelist_treatment,
  name = "optima_pc_trial",
  exit = "event_start_date"
) |>
  # first entry
  CohortConstructor::requireIsFirstEntry(name = "optima_pc_trial") |>
  # only first entry per cohort
  dplyr::group_by(.data$subject_id) |>
  dplyr::slice_min(.data$cohort_start_date) |>
  dplyr::compute(name = "optima_pc_trial") |>
  omopgenerics::recordCohortAttrition(
    reason = "Remove individuals with a prostatectomy",
    cohortId = "EBRT"
  ) |>
  omopgenerics::recordCohortAttrition(
    reason = "Remove individuals with radiotherapy before",
    cohortId = "Radical prostatectomy"
  ) |>
  # prior record of prostate cancer
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "prostate_cancer_age_50_69",
    window = c(-180, 0),
    intersections = c(1, Inf)
  ) |>
  #  require psa value in the last 180 days
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "psa_trial",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |>
  # require stage any time prior (m0 can be after)
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "stage",
    window = c(-Inf, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |>
  # to consider removing this line
  omopgenerics::recordCohortAttrition(reason = "at least one measurement of both cT1-T2 and M0 or of Stage I-II any day before cohort start date") |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Malignancy except non-melanoma skin cancer" = codelist$`Malignancy except non-melanoma skin cancer`),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("copd" = codelist$`[OPTIMA PCa RQ 4] COPD`) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("heart failure" = codelist$`[OPTIMA PCa RQ4] Heart failure`) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("mi" = codelist$`[OPTIMA PCa RQ4] Myocardial infarction`) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("stroke" = codelist$`[OPTIMA PCa RQ4] Stroke`) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Kidney dialysis or transplantation" = codelist$`[OPTIMA PCa RQ4] Kidney dialysis or transplantation`),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Bilateral hip replacement" = codelist$`[OPTIMA PCa RQ4] Bilateral hip replacement`),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::renameCohort(cohortId = c(1, 2), newCohortName = c("ebrt_trial", "radical_prostatectomy_trial"))


# cdm <- cdm$treatment |> omopgenerics::bind(cdm$no_treatment, name = "optima_pc_trial")



# rwd ----

cdm$optima_pc_rwd <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_rwd") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>

  CohortConstructor::requirePriorObservation(minPriorObservation = 365) |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
                                             window = c(-Inf,Inf),
                                             intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(targetCohortTable = "stage",
                                            window = c(-180, 0),
                                            intersections = c(1, Inf)) |>

  dplyr::left_join(cdm$psa_values |> dplyr::select(subject_id, value_as_number, unit_source_value), by = "subject_id") |>

  dplyr::compute(name = "optima_pc_rwd") |>

  CohortConstructor::renameCohort(cohortId = c(1, 2), newCohortName = c("ebrt_rwd", "radical_prostatectomy_rwd"))


# cdm$no_treatment <- cdm$prostate_cancer_first_diagnosis |>
#   CohortConstructor::requirePriorObservation(minPriorObservation = 365) |>
#   CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = c(codelist_treatment$EBRT, codelist_treatment$`Radical prostatectomy`)),
#                                              window = c(-Inf, 0),
#                                              intersections = 0) |>
#   CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = c(codelist_treatment$EBRT, codelist_treatment$`Radical prostatectomy`)),
#                                              window = c(-Inf, 180),
#                                              intersections = 0) |>
#   CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = c(codelist_treatment$EBRT, codelist_treatment$`Radical prostatectomy`)),
#                                              window = c(-Inf, 365),
#                                              intersections = 0) |>
#   CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = c(codelist_treatment$EBRT, codelist_treatment$`Radical prostatectomy`)),
#                                              window = c(-Inf, Inf),
#                                              intersections = 0) |>
#   dplyr::compute(name = "no_treatment") |>
#   CohortConstructor::renameCohort(cohortId = 1, newCohortName = "no_treatment_rwd")


# cdm <- cdm$treatment |> omopgenerics::bind(cdm$no_treatment, name = "optima_pc_rwd")

# registry ----

# cdm2 <- cdm
# cdm2$condition_occurrence <- cdm$condition_occurrence |>
#   dplyr::filter(.data$condition_type_concept_id %in% c(32815, 32828, 32835, 32879) )
#
# cdm$prostate_cancer_diagnosis_registry <- CohortConstructor::conceptCohort(
#   cdm = cdm2,
#   conceptSet = list(cancer = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
#   name = "prostate_cancer_diagnosis_registry"
# )
#
# cdm$optima_pc_registry <- cdm$optima_pc_rwd |> CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_diagnosis_registry",
#                                                                                          window = c(-Inf,Inf),
#                                                                                          intersections = c(1, Inf)) |>
#   CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_diagnosis_registry",
#                                             window = c(-180,180),
#                                             intersections = c(1, Inf)) |>
#   CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_diagnosis_registry",
#                                             window = c(-180,0),
#                                             intersections = c(1, Inf)) |>
#   dplyr::compute(name = "optima_pc_registry") |>
#   CohortConstructor::renameCohort(cohortId = c(1, 2, 3), newCohortName = c("ebrt_registry", "radical_prostatectomy_registry", "no_treatment_registry"))


# delete not needed cohorts ----
#omopgenerics::dropSourceTable(cdm = cdm, c("treatment", "no_treatment"))
