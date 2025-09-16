source("MainStudy/Model/functions.R")


output_directory = here::here("MainStudy/Results")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |> unlist() |> unname()

### Lasso ----

cdm <- addVariables(cdm)
cdm <- longDataFromCohort(cdm)

frequent_concepts <- getFrequentConcepts(cohort = cdm$optima_pc_rwd_long, excluded_codes = excluded_codes)

cdm <- visitsCount(cdm)
cdm$optima_pc_rwd_long <- cdm$optima_pc_rwd_long |>
  PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf)))

wide_data <- getWideData(cdm$optima_pc_rwd_long, frequent_concepts, cdm$optima_pc_rwd_visits)

selectedFeatures <- getSelectedFeatures(wide_data = wide_data,
                                        directory = paste0(output_directory, "/Lasso"),
                                        cdm = cdm,
                                        cdm_name = dbName)



### Matching ----

matched_data <- getMatchedData(selectedFeatures = selectedFeatures,
                               wide_data = wide_data,
                               directory = paste0(output_directory, "/Matching"),
                               cdm_name = dbName)
matched_data <- matched_data |>
  dplyr::select("pair_id", "cohort_definition_id", "subject_id", "cohort_start_date",  "cohort_end_date")

cdm <- omopgenerics::insertTable(cdm = cdm, name = "optima_pc_rwd_matched", table = matched_data)

cdm$optima_pc_rwd_matched <- omopgenerics::newCohortTable(table = cdm$optima_pc_rwd_matched)


### Outcome model ----

codelist_ic <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/InclusionCriteria", "csv")

codelist_progression <- list("t3_t4" = codelist_ic$t3_t4 , "stage3_4" = codelist_ic$stage3_4)


cdm$progression <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_progression, name = "progression") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m1" = codelist_ic$m1),
                                             cohortId = "t3_t4",
                                             window = c(-365, 365),
                                             intersections = c(1, Inf),
                                             name = "progression") |>
  CohortConstructor::unionCohorts(name = "progression")

outcome_codelist <- omopgenerics::importCodelist(here::here("Codelist/Outcomes"), type = "csv")
outcomes <- clean_names(names(outcome_codelist))
names(outcome_codelist) <- outcomes

cdm[["survival_data"]] <- cdm[["optima_pc_rwd_matched" ]] |>
  PatientProfiles::addDeathDays(deathDaysName = "death", name = "survival_data") |>
  PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation", name = "survival_data") |>
  PatientProfiles::addCohortIntersectDays(targetCohortTable = "progression", nameStyle = "progression", name = "survival_data" ) |>
  dplyr::mutate(
    death = dplyr::coalesce(.data[["death"]], 999999L),
    progression = dplyr::coalesce(.data[["progression"]], 999999L)
  ) |>
  dplyr::mutate(censor_time = case_when(
    .data$death <= .data$end_of_observation & .data$death <= .data$progression ~ .data$death,
    .data$progression <= .data$end_of_observation ~ .data$progression,
    .default = .data$end_of_observation
  ),
  censor_reason = case_when(
    .data$censor_time == .data$progression ~ "progression",
    .data$censor_time == .data$death ~ "death",
    .data$censor_time == .data$end_of_observation ~ "end of observation",
    .default = "error"
  )
  ) |>
  dplyr::mutate(
    treatment = dplyr::case_when(
      .data$cohort_definition_id == 1L ~ "EBRT",
      .data$cohort_definition_id == 2L ~ "RP",
      TRUE ~ as.character(.data$cohort_definition_id)
    )
  ) |>
  dplyr::compute(name = "survival_data")

for (out in outcomes){

  cdm[["survival_data"]] <- cdm[["survival_data"]] |>
    PatientProfiles::addConceptIntersectDays(conceptSet = list(out = outcome_codelist[[out]]),
                                             nameStyle = out,
                                             name = "survival_data") |>
    dplyr::mutate(
      !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
    )

}
survival_data <-  cdm[["survival_data"]] |>
  dplyr::collect()
outcomes <- c("death", "progression", outcomes)
result <- outcomes |>
  purrr::map(\(out) {
     outcomeModel(survival_data = survival_data, outcome = out)
  }) |>
  bindResults(cdmName = dbName)

omopgenerics::exportSummarisedResult(result, path = paste0(output_directory, "/Survival"), fileName = "survival_results_{cdm_name}.csv")


