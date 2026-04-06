codelist_treatment <- list("EBRT" = codelist$`[OPTIMA RQ4] EBRT`, "Radical prostatectomy" = codelist$`[OPTIMA RQ4] Radical prostatectomy`)

codelist_stage <- list("cT1-T2" = codelist$`[OPTIMA PCa RQ4] cT1-T2` , "Stage I-II" = codelist$`[OPTIMA PCa RQ4] Stage I-II`)

# concept cohorts ----




cdm$stage1 <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_stage, name = "stage1") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("MO" = codelist$`[OPTIMA PCa RQ4] M0`),
                                             window = c(-Inf, Inf),
                                             cohortId = "c_t1_t2",
                                             intersections = c(1, Inf)) |>
  CohortConstructor::unionCohorts()

cdm$stage2 <- CohortConstructor::conceptCohort(cdm, conceptSet = list("c_t1_t2" = codelist$`[OPTIMA PCa RQ4] cT1-T2`), name = "stage2") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("MO" = codelist$`[OPTIMA PCa RQ4] M0`),
                                             window = c(-Inf, Inf),
                                             cohortId = "c_t1_t2",
                                             intersections = c(1, Inf))



# tte ----


cdm$initial_cohort <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "initial_cohort") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>
  dplyr::compute(name = "initial_cohort") |>

  CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_age_50_69",
                                            window = c(-180,0),
                                            intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "psa_trial",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |> dplyr::compute(name = "initial_cohort")

cdm$with_stage_concepts <- cdm$initial_cohort |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "stage1",
    window = c(-Inf, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |> dplyr::compute(name = "with_stage_concepts")
cdm$no_stage_concepts <- cdm$initial_cohort |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "stage2",
    window = c(-Inf, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |> dplyr::compute(name = "no_stage_concepts")


cdm$no_stage_concepts_six_months <- cdm$initial_cohort |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "stage2",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
  ) |> dplyr::compute(name = "no_stage_concepts_six_months")

attr_stage_concepts <- CohortCharacteristics::summariseCohortAttrition(cdm$with_stage_concepts)
attr_no_stage_concepts <- CohortCharacteristics::summariseCohortAttrition(cdm$no_stage_concepts)
attr_no_stage_concepts_six_months <- CohortCharacteristics::summariseCohortAttrition(cdm$no_stage_concepts_six_months)

CohortCharacteristics::tableCohortAttrition(attr_stage_concepts)






