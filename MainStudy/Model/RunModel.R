source("MainStudy/Model/functions.R")


output_directory <- here::here("MainStudy/Results")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |>
  unlist() |>
  unname()



cohorts <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf")
#c( "optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")
for (cohort_name in cohorts) {
  cohort_name_long <- paste(cohort_name, "long", sep = "_")
  cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
  cohort_name_matched <- paste(cohort_name, "matched", sep = "_")

  result <- list()
  ### Lasso ----

  cdm <- addVariables(cdm, cohort_name = cohort_name)
  cdm <- longDataFromCohort(cdm, cohort_name = cohort_name, excluded_codes = excluded_codes)

  frequent_concepts <- getFrequentConcepts(cohort = cdm[[cohort_name_long]])

  cdm <- visitsCount(cdm, cohort_name = cohort_name)
  cdm[[cohort_name_long]] <- cdm[[cohort_name_long]] |>
    PatientProfiles::addAgeQuery(ageGroup = list(c(0, 50), c(51, 55), c(56, 60), c(61, 65), c(66, 70), c(71, 75), c(76, 80), c(81, Inf)))

  wide_data <- getWideData(cohort = cdm[[cohort_name_long]], frequent_concepts = frequent_concepts, visits = cdm[[cohort_name_visits]])
  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0), )

  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0), )

  x <- getSelectedFeatures(
    wide_data = wide_data,
    cdm = cdm,
    cdm_name = dbName
  )



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
    directory = paste0(output_directory, "/Matching"),
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
    dplyr::select("pair_id", "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", dplyr::starts_with("latest"))

  cdm <- omopgenerics::insertTable(cdm = cdm, name = cohort_name_matched, table = matched_data)

  cdm[[cohort_name_matched]] <- omopgenerics::newCohortTable(table = cdm[[cohort_name_matched]])


  ### Outcome model ----

  outcome_codelist <- omopgenerics::importCodelist(here::here("Codelist/Outcomes"), type = "csv")
  outcomes <- clean_names(names(outcome_codelist))
  names(outcome_codelist) <- outcomes

  death_codelist <- omopgenerics::importCodelist(here::here("Codelist/CauseOfDeath"), type = "csv")
  death_pc_codes <- death_codelist[["prostate_cancer_death"]]
  death_cvd_codes <- death_codelist[["cvd_death"]]

  cdm[["survival_data"]] <- cdm[[cohort_name_matched]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", name = "survival_data", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation", name = "survival_data") |>
    dplyr::mutate(
      death = dplyr::coalesce(.data[["death"]], 999999L)
    ) |>
    PatientProfiles::addTableIntersectField(tableName = "death", field = "cause_concept_id", nameStyle = "cause_of_death") |>
    dplyr::mutate(
      death_pc = dplyr::if_else(!is.na(.data$cause_of_death) & .data$cause_of_death %in% death_pc_codes, .data$death, 999999L),
      death_cvd = dplyr::if_else(!is.na(.data$cause_of_death) & .data$cause_of_death %in% death_cvd_codes, .data$death, 999999L)
    ) |>
    dplyr::select(!"cause_of_death") |>
    dplyr::mutate(
      censor_time = case_when(
        .data$death <= .data$end_of_observation ~ .data$death,
        .default = .data$end_of_observation
      ),
      censor_reason = case_when(
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

  for (out in outcomes) {
    washout_name <- paste0(out, "_washout")
    window <- list(c(1, Inf))
    if (out == "androgen_deprivation") {
      window <- list(c(181, Inf))
    } else {
      window <- list(c(1, Inf))
    }
    cdm[["survival_data"]] <- cdm[["survival_data"]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = window,
        nameStyle = out,
        name = "survival_data"
      ) |>
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = list(c(-365, 0)),
        targetStartDate = "event_end_date",
        name = "survival_data",
        nameStyle = washout_name
      ) |>
      dplyr::mutate(
        !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
      )


    pairs <- cdm[["survival_data"]] |>
      dplyr::filter(.data[[washout_name]] == 1L) |>
      dplyr::pull(.data$pair_id)

    cdm[["survival_data"]] <- cdm[["survival_data"]] |>
      dplyr::mutate(!!rlang::sym(out) := dplyr::if_else(.data$pair_id %in% pairs, 999999L, .data[[out]])) |>
      dplyr::select(!dplyr::all_of(washout_name)) |>
      dplyr::compute("survival_data")
  }
  survival_data <- cdm[["survival_data"]] |>
    dplyr::collect()
  outcomes <- c("death", "death_cvd", "death_pc", outcomes)
  res_outcomes <- outcomes |>
    purrr::map(\(out) {
      outcomeModel(survival_data = survival_data, outcome = out)
    })
  result[["survival"]] <- res_outcomes |>
    bindResults(cdmName = dbName, cohort_name = cohort_name)

  nco_codelist <- omopgenerics::importCodelist(here::here("Codelist/NCO"), type = "csv")
  negative_control_outcomes <- clean_names(names(nco_codelist))
  names(nco_codelist) <- negative_control_outcomes


  cdm[["nco"]] <- cdm[[cohort_name_matched]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", name = "nco", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation", name = "nco") |>
    dplyr::mutate(
      death = dplyr::coalesce(.data[["death"]], 999999L)
    ) |>
    dplyr::mutate(
      censor_time = case_when(
        .data$death <= .data$end_of_observation ~ .data$death,
        .default = .data$end_of_observation
      ),
      censor_reason = case_when(
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
    dplyr::compute(name = "nco")

  for (nco in negative_control_outcomes) {

    cdm[["nco"]] <- cdm[["nco"]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(nco = nco_codelist[[nco]]),
        window = list(c(1, Inf)),
        nameStyle = nco,
        name = "nco"
      )
  }
  nco_table <- cdm[["nco"]] |>
    dplyr::collect()

  res_nco <- negative_control_outcomes |>
    purrr::map(\(nco) {
      outcomeModel(survival_data = nco_table, outcome = nco)
    })
  result[["nco"]] <- res_nco |>
    bindResults(cdmName = dbName, cohort_name = cohort_name)
  set <- omopgenerics::settings(result[["nco"]]) |>
    dplyr::mutate(result_type = paste0("nco_", .data$result_type))

  result[["nco"]] <- omopgenerics::newSummarisedResult(result[["nco"]], settings = set)


  result_to_export <- omopgenerics::bind(result)
  omopgenerics::exportSummarisedResult(result, path = paste0(output_directory, "/Survival"), fileName = paste0("results_{cdm_name}_", cohort_name, ".csv"))
}
