source("MainStudy/Model/functions.R")


output_directory = here::here("MainStudy/Results")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |> unlist() |> unname()

cohort_name = "optima_pc_rwd"
cohort_name_long <- paste(cohort_name, "long", sep = "_")
cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
cohort_name_matched <- paste(cohort_name, "matched", sep = "_")

result <- list()
### Lasso ----

cdm <- addVariables(cdm, cohort_name = cohort_name)
cdm <- longDataFromCohort(cdm, cohort_name = cohort_name)

frequent_concepts <- getFrequentConcepts(cohort = cdm[[cohort_name_long]], excluded_codes = excluded_codes)

cdm <- visitsCount(cdm, cohort_name = cohort_name)
cdm[[cohort_name_long]] <- cdm[[cohort_name_long]] |>
  PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf)))

wide_data <- getWideData(cdm[[cohort_name_long]], frequent_concepts, cdm[[cohort_name_visits]])

x <- getSelectedFeatures(wide_data = wide_data,
                         directory = paste0(output_directory, "/Lasso"),
                         cdm = cdm,
                         cdm_name = dbName)



result[["density_points"]] <- x$density_points |>
  dplyr::mutate(
    strata_name  = "treatment",
    strata_level = as.character(.data$treatment),
    idx          = dplyr::row_number(),
    density_x    = .data$x,
    density_y    = .data$y
  ) |>
  tidyr::pivot_longer(
    cols = c(density_x, density_y),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(
    estimate_name = paste0(.data$estimate_name, "_", .data$idx),
    estimate_value = sprintf("%.4f", .data$estimate_value),
    estimate_type = "numeric",
    cohort = cohort_name,
    cdm_name = dbName,
    variable_name  = NA_character_,
    variable_level = NA_character_,
    result_id = 1L

  ) |>
  dplyr::select(!c("treatment", "x", "y", "idx")) |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  omopgenerics::uniteAdditional() |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "distribution_ps"))

result[["selected_features"]] <- x$selected_features |>
  dplyr::mutate(coefficient = sprintf("%.4f", .data$coefficient),
                cohort = cohort_name,
                result_type = "selected_features") |>
  omopgenerics::transformToSummarisedResult(group = "cohort", strata = "variable", additional = c("concept_name","domain_id"),
                                            estimates = "coefficient", settings = "result_type") |>
  dplyr::mutate(cdm_name = dbName)

asmd <- computeASMD(wide_data = wide_data, features = x$selected_columns)

result[["asmd"]] <- asmd |>
  tidyr::pivot_longer(
    cols = c(smd, asmd),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(cohort = cohort_name,
                cdm_name = dbName,
                estimate_value = sprintf("%.3f", .data$estimate_value),
                estimate_type = "numeric",
                variable_name = "covariate",
                variable_level = .data$covariate,
                result_id = 1L) |>
  omopgenerics::uniteAdditional(cols = c("reference", "comparator")) |>
  omopgenerics::uniteStrata() |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  dplyr::select(!"covariate") |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))

### Matching ----

matched_data <- getMatchedData(selectedFeatures = x$selected_columns,
                               wide_data = wide_data,
                               directory = paste0(output_directory, "/Matching"),
                               cdm_name = dbName)

asmd_matched <- computeASMD(wide_data = matched_data, features = x$selected_columns)

result[["asmd_matched"]] <- asmd_matched |>
  tidyr::pivot_longer(
    cols = c(smd, asmd),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(cohort = cohort_name_matched,
                cdm_name = dbName,
                estimate_value = sprintf("%.3f", .data$estimate_value),
                estimate_type = "numeric",
                variable_name = "covariate",
                variable_level = .data$covariate,
                result_id = 1L) |>
  omopgenerics::uniteAdditional(cols = c("reference", "comparator")) |>
  omopgenerics::uniteStrata() |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  dplyr::select(!"covariate") |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))


matched_data <- matched_data |>
  dplyr::select("pair_id", "cohort_definition_id", "subject_id", "cohort_start_date",  "cohort_end_date")

cdm <- omopgenerics::insertTable(cdm = cdm, name = cohort_name_matched, table = matched_data)

cdm[[cohort_name_matched]] <- omopgenerics::newCohortTable(table = cdm[[cohort_name_matched]])


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

cdm[["survival_data"]] <- cdm[[cohort_name_matched]] |>
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
res_outcomes <- outcomes |>
  purrr::map(\(out) {
     outcomeModel(survival_data = survival_data, outcome = out)
  })
result[["survival"]] <- res_outcomes |>
  bindResults(cdmName = dbName, cohort_name = cohort_name)


result_to_export <- omopgenerics::bind(result)
omopgenerics::exportSummarisedResult(result, path = paste0(output_directory, "/Survival"), fileName = paste0("results_{cdm_name}_", cohort_name,".csv"))


