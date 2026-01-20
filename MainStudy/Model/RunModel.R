source("Model/functions.R")


output_directory <- here::here("Results")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |>
  unlist() |>
  unname()



cohorts <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf")
#c( "optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")

for (cohort_name in cohorts) {
  n_rows <- cdm[[cohort_name]]|>
    dplyr::tally() |>
    dplyr::pull()
  if (n_rows>0) {
  cohort_name_long <- paste(cohort_name, "long", sep = "_")
  cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
  cohort_name_matched <- paste(cohort_name, "matched", sep = "_")

  result <- list()
  ### Lasso ----

  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::compute(name = cohort_name)

  cdm <- longDataFromCohort(cdm, cohort_name = cohort_name, excluded_codes = excluded_codes)

  frequent_concepts <- getFrequentConcepts(cohort = cdm[[cohort_name_long]])

  cdm <- visitsCount(cdm, cohort_name = cohort_name)

  cdm[[cohort_name_long]] <- cdm[[cohort_name_long]] |>
    addVariables() |>
    PatientProfiles::addAge()

  wide_data <- getWideData(cohort = cdm[[cohort_name_long]], frequent_concepts = frequent_concepts, visits = cdm[[cohort_name_visits]])

  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 0, 1))

  x <- getSelectedFeatures(
    wide_data = wide_data,
    cdm = cdm,
    cdm_name = dbName
  )

  if(length( x$selected_columns) >0 ) {

  result[["density_points"]] <- x$density_points |>
    dplyr::mutate(
      strata_name  = "treatment",
      strata_level = as.character(.data$treatment),
      idx          = dplyr::row_number(),
      density_x    = .data$x,
      density_y    = .data$y
    ) |>
    dplyr::mutate(variable_level = paste("density", .data$idx, sep = "_")) |>
    tidyr::pivot_longer(
      cols = c(density_x, density_y),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      estimate_value = sprintf("%.4f", .data$estimate_value),
      estimate_type = "numeric",
      cohort = cohort_name,
      cdm_name = dbName,
      variable_name = "Propensity score distribution",
      result_id = 1L
    ) |>
    dplyr::select(!c("treatment", "x", "y", "idx")) |>
    omopgenerics::uniteGroup(cols = "cohort") |>
    omopgenerics::uniteAdditional() |>
    omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "distribution_ps"))

  result[["selected_features"]] <- x$selected_features |>
    dplyr::mutate(
      coefficient = sprintf("%.4f", .data$coefficient),
      cohort = cohort_name,
      result_type = "selected_features",
      variable_name = "event X=1",
      variable_level = .data$event
    ) |>
    omopgenerics::transformToSummarisedResult(
      group = "cohort", strata = "variable", additional = c("concept_name", "domain_id", "window"),
      estimates = "coefficient", settings = "result_type"
    ) |>
    dplyr::mutate(cdm_name = dbName)

  asmd <- computeASMD(wide_data = wide_data, features = x$selected_columns)

  result[["asmd"]] <- asmd |>
    tidyr::pivot_longer(
      cols = c(smd, asmd),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      cohort = cohort_name,
      cdm_name = dbName,
      estimate_value = sprintf("%.3f", .data$estimate_value),
      estimate_type = "numeric",
      variable_name = "covariate",
      variable_level = .data$covariate,
      result_id = 1L
    ) |>
    omopgenerics::uniteAdditional(cols = c("event", "comparator")) |>
    omopgenerics::uniteStrata() |>
    omopgenerics::uniteGroup(cols = "cohort") |>
    dplyr::select(!"covariate") |>
    omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))

  ### Matching ----

 matched_data <- getMatchedData(
    selectedFeatures = x$selected_columns,
    wide_data = wide_data,
    cdm_name = dbName
  )

  asmd_matched <- computeASMD(wide_data = matched_data, features = x$selected_columns)

  result[["asmd_matched"]] <- asmd_matched |>
    tidyr::pivot_longer(
      cols = c(smd, asmd),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      cohort = cohort_name_matched,
      cdm_name = dbName,
      estimate_value = sprintf("%.3f", .data$estimate_value),
      estimate_type = "numeric",
      variable_name = "covariate",
      variable_level = .data$covariate,
      result_id = 1L
    ) |>
    omopgenerics::uniteAdditional(cols = c("event", "comparator")) |>
    omopgenerics::uniteStrata() |>
    omopgenerics::uniteGroup(cols = "cohort") |>
    dplyr::select(!"covariate") |>
    omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))


  matched_data <- matched_data |>
    dplyr::select("pair_id", "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", dplyr::starts_with("latest"), dplyr::starts_with("psa"))

  cdm <- omopgenerics::insertTable(cdm = cdm, name = cohort_name_matched, table = matched_data)

  cdm[[cohort_name_matched]] <- omopgenerics::newCohortTable(table = cdm[[cohort_name_matched]])

  result[["characterisation_matched_cohort"]] <- cohortCharacterisation(cdm = cdm, cohort_name = cohort_name_matched)

  ### Outcome model ----

  outcome_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "Outcomes"), type = "csv")
  outcomes <- clean_names(names(outcome_codelist))
  names(outcome_codelist) <- outcomes

  death_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "CauseOfDeath"), type = "csv")
  death_pc_codes <- death_codelist[["prostate_cancer_death"]]
  death_cvd_codes <- death_codelist[["cvd_death"]]


  cdm[[cohort_name_matched]] <- deathSurvival(cdm = cdm,
                                          cohort_name = cohort_name_matched) |>
    addCauseOfDeath(death_pc_codes = death_pc_codes,
                    death_cvd_codes = death_cvd_codes)|>
    dplyr::compute(name = cohort_name_matched)

  for (out in c(outcomes, "type2_diabetes")) {

    cdm[[cohort_name_matched]] <- cdm[[cohort_name_matched]] |>
      addOutcome(outcome = out, outcome_codelist = outcome_codelist)
  }
  survival_data <- cdm[[cohort_name_matched]] |>
    dplyr::collect()
  outcomes <- c("death", "death_cvd", "death_pc", "type2_diabetes", outcomes)
  res_outcomes <- outcomes |>
    purrr::map(\(out) {
      outcomeModel(survival_data = survival_data, outcome = out)
    })
  result[["survival"]] <- res_outcomes |>
    bindResults(cdmName = dbName, cohort_name = cohort_name)
  ### NCO ----
  nco_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "NCO"), type = "csv")
  negative_control_outcomes <- clean_names(names(nco_codelist))
  names(nco_codelist) <- negative_control_outcomes


  for (nco in negative_control_outcomes) {

    cdm[[cohort_name_matched]] <- cdm[[cohort_name_matched]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(nco = nco_codelist[[nco]]),
        window = list(c(1, Inf)),
        nameStyle = nco,
        name = cohort_name_matched
      )
  }
  nco_table <- cdm[[cohort_name_matched]] |>
    dplyr::collect()

  res_nco <- negative_control_outcomes |>
    purrr::map(\(nco) {
      NCOModel(survival_data = nco_table, outcome = nco)
    })
  result[["nco"]] <- res_nco |>
    bindResults(cdmName = dbName, cohort_name = cohort_name)
  set <- omopgenerics::settings(result[["nco"]]) |>
    dplyr::mutate(result_type = paste0("nco_", .data$result_type))

  result[["nco"]] <- omopgenerics::newSummarisedResult(result[["nco"]], settings = set)

   result_to_export <- omopgenerics::bind(result)
   omopgenerics::exportSummarisedResult(result, path = output_directory, fileName = paste0("results_{cdm_name}_", cohort_name, "_{date}.csv"))
  }
  }
}
