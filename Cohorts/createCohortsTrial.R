#cohort trial

folder_path <- here::here("Diagnostics/Cohorts/codelists/total")


codelist <- omopgenerics::importCodelist(paste0(folder_path), "csv")

codelist_treatment <- list("EBRT" = codelist$`[OPTIMA RQ4] EBRT`, "Radical prostatectomy" = codelist$`[OPTIMA RQ4] Radical prostatectomy`)
codelist_stage <- list("cT1-T2" = codelist$`[OPTIMA PCa RQ4] cT1-T2` , "Stage I-II" = codelist$`[OPTIMA PCa RQ4] Stage I-II`)

cdm$psa_cohort <- CohortConstructor::measurementCohort(cdm,
                                                       conceptSet = list("PSA" = codelist$`[OPTIMA PCa RQ4] PSA`),
                                                       name = "psa_cohort",
                                                       valueAsNumber = list(c(3, 19.99)) )

cdm$stage <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_stage, name = "stage") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("MO" = codelist$`[OPTIMA PCa RQ4] M0`),
                                             window = c(-Inf, Inf),
                                             cohortId = "c_t1_t2",
                                             intersections = c(1, Inf)) |>
  CohortConstructor::unionCohorts(cohortId = c(1,2), name = "stage")


cdm$prostate_cancer_cohort <- CohortConstructor::conceptCohort(cdm,
                                                               conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
                                                               name = "prostate_cancer_cohort" ) |>
  CohortConstructor::requireDemographics(sex = "Male",
                                         ageRange = list(c(50, 69)),
                                         minPriorObservation = 365,
                                         indexDate = "cohort_start_date") |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "psa_cohort",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "stage",
    window = c(-Inf, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
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
    )|>
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
  )

attrition(cdm$prostate_cancer_cohort)

cdm$optima_pc_tte <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_tte", subsetCohort = "prostate_cancer_cohort") |>
  CohortConstructor::requireIsFirstEntry()

attrition(cdm$optima_pc_tte)
