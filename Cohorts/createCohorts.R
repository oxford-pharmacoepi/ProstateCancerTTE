
# import concepts ----

folder_path <-"~/ProstateCancerTTE/Codelist/InclusionCriteria"

codelist <- omopgenerics::importCodelist(paste0(folder_path), "csv")

codelist_treatment <- list("ebrt" = codelist$ebrt, "radical_prostatectomy" = codelist$radical_prostatectomy)

codelist_early_stage <- list("t1_t2" = codelist$t1_t2 , "stage1_2" = codelist$stage1_2)

codelist_advanced_stage <- list("t3_t4" = codelist$t3_t4 , "stage3_4" = codelist$stage3_4)

if (grepl("gold", dbName)) {
  dir_excluded_subjects <- "~/ProstateCancerTTE/SubjectsToRemove/cdm_gold_p22_001867_person_id_all.csv"
  excluded_subjects <- utils::read.csv(dir_excluded_subjects, header = TRUE)$person_id
} else if (grepl("aurum", dbName)) {
  dir_excluded_subjects <- "~/ProstateCancerTTE/SubjectsToRemove/cdm_aurum_p22_001867_person_id_all.csv"
  excluded_subjects <- utils::read.csv(dir_excluded_subjects, header = TRUE)$person_id
} else {
  excluded_subjects <- c()
}


# concept cohorts ----

cdm$prostate_cancer_age_50_69 <- CohortConstructor::conceptCohort(cdm,
                                                                  conceptSet = list("Prostate cancer" = codelist$prostate_cancer),
                                                                  name = "prostate_cancer_age_50_69" ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireDemographics(sex = "Male",
                                         ageRange = list(c(50, 69)),
                                         minPriorObservation = 365,
                                         indexDate = "cohort_start_date")

cdm$psa_values <- CohortConstructor::measurementCohort(cdm,
                                                conceptSet = list(psa = codelist$psa),
                                                name = "psa_values",
                                                valueAsNumber = list(c(-Inf, Inf))) |>
  PatientProfiles::addConceptIntersectField(conceptSet = list(psa = codelist$psa),
                                            field = "value_as_number",
                                            indexDate = "cohort_start_date",
                                            order = "first",
                                            window = c(0,0),
                                            name = "psa_values",
                                            allowDuplicates = TRUE,
                                            nameStyle = "{field}") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = unname(unlist(codelist_treatment))),
                                             window = c(0, 180) ) |>
  CohortConstructor::requireIsLastEntry()


cdm$psa_trial <- cdm$psa_values |>
  dplyr::filter(.data$value_as_number <= 19.99 & .data$value_as_number >=3) |>
  dplyr::compute(name = "psa_trial")


cdm$early_stage <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_early_stage, name = "early_stage") |>
  PatientProfiles::addConceptIntersectDate(conceptSet = list("treatment" = unname(unlist(codelist_treatment))), nameStyle = "treatment_date", name = "early_stage")

cdm$early_stage_trial <- cdm$early_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m0" = codelist$m0),
                                             window = c(-180, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t1_t2",
                                             intersections = c(1, Inf),
                                             name = "early_stage_trial"
                                             ) |>
  CohortConstructor::unionCohorts(name = "early_stage_trial")

cdm$early_stage_rwd <- cdm$early_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m0" = codelist$m0),
                                             window = c(-Inf, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t1_t2",
                                             intersections = c(1, Inf),
                                             name = "early_stage_rwd"
  ) |>
  CohortConstructor::unionCohorts(name = "early_stage_rwd")

cdm$advanced_stage <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_advanced_stage, name = "advanced_stage") |>
  PatientProfiles::addConceptIntersectDate(conceptSet = list("treatment" = unname(unlist(codelist_treatment))), nameStyle = "{concept_name}_date", name = "advanced_stage")

cdm$advanced_stage_trial <- cdm$advanced_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m1" = codelist$m1),
                                             window = c(-180, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t3_t4",
                                             intersections = c(1, Inf),
                                             name = "advanced_stage_trial") |>
  CohortConstructor::unionCohorts( name = "advanced_stage_trial")

cdm$advanced_stage_rwd <- cdm$advanced_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m1" = codelist$m1),
                                             window = c(-Inf, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t3_t4",
                                             intersections = c(1, Inf),
                                             name = "advanced_stage_rwd") |>
  CohortConstructor::unionCohorts( name = "advanced_stage_rwd")


# tte ----

cdm$optima_pc_trial <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_trial") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>

  CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_age_50_69",
                                            window = c(-180,0),
                                            intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "early_stage_trial",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "advanced_stage_trial",
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |>
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
    conceptSet = list("copd" = codelist$copd) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("heart failure" = codelist$heart_failure) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("mi" = codelist$mi) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("stroke" = codelist$stroke) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Kidney dialysis or transplantation" = codelist$kidney_dialysis_transplantation),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Bilateral hip replacement" = codelist$bilateral_hip_replacement),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  dplyr::filter(!(.data$subject_id %in% excluded_subjects)) |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects with records related to female conditions") |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects both treatment the same day") |>
  dplyr::left_join(cdm$psa_trial |> dplyr::select("subject_id", "psa_value" = "value_as_number"), by = "subject_id") |>
  dplyr::compute(name = "optima_pc_trial") |>
  CohortConstructor::renameCohort(cohortId = c(1, 2), newCohortName = c("ebrt_trial", "radical_prostatectomy_trial"))
# rwd ----

cdm$optima_pc_rwd <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_rwd") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>

  dplyr::compute(name = "optima_pc_rwd") |>

  CohortConstructor::requirePriorObservation(minPriorObservation = 365) |>


  CohortConstructor::requireConceptIntersect(conceptSet = list("Prostate cancer conditions" = codelist$prostate_cancer),
                                             window = c(-Inf, 0),
                                             intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(targetCohortTable = "early_stage_rwd",
                                            window = c(-Inf, 0),
                                            intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "advanced_stage_rwd",
    window = c(-Inf, 0),
    intersections = 0) |>

  dplyr::left_join(cdm$psa_values |> dplyr::select(subject_id, value_as_number), by = "subject_id") |>
  dplyr::filter(!(.data$subject_id %in% excluded_subjects)) |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects with records related to female conditions") |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects both treatment the same day") |>
  dplyr::compute(name = "optima_pc_rwd") |>
  CohortConstructor::renameCohort(cohortId = c(1, 2), newCohortName = c("ebrt_rwd", "radical_prostatectomy_rwd"))


