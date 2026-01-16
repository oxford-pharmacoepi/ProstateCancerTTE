host <- Sys.getenv("HOST")
port <- Sys.getenv("PORT")
username <- Sys.getenv("USER")
password <- Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)



source("Model/functions.R")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |>
  unlist() |>
  unname()
output_directory <- here::here("Results")
cohort <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf")
#cohort <- c("optima_pc_trial_2010_2020","optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")
for (cohort_name in cohort) {
  cohort_name_long <- paste(cohort_name, "long", sep = "_")
  cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
  cohort_name_matched <- paste(cohort_name, "matched", sep = "_")
  merged_matched_cohort_name <- paste("merged", cohort_name_matched, sep = "_")

  if (grepl("rwd", cohort_name)){

    gleason_cohort <- "gleason_rwd"

    n_status_cohort <- "n_status_rwd"

    t_status_cohort <- "t_status_rwd"

    psa_cohort <- "psa_values_rwd"


  } else if (grepl("trial", cohort_name)) {
    gleason_cohort <- "gleason_trial"

    n_status_cohort <- "n_status_trial"

    t_status_cohort <- "t_status_trial"

    psa_cohort <- "psa_values_trial"
  }


  result <- list()

  ### gold ----
  dbName_gold <- "gold_pc"
  con_gold <- dbConnect(
    drv = Postgres(),
    dbname = "cdm_gold_p22_001867",
    host = host,
    port = port,
    user = username,
    password = password
  )


  cdm_g <- cdmFromCon(
    con = con_gold, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_gold,
    cohortTables = c(cohort_name, cohort_name_long, cohort_name_visits, t_status_cohort, n_status_cohort, psa_cohort, gleason_cohort, "medications", "conditions",
                     "type2_diabetes")
  )


  cdm_g$observation_period <- cdm_g$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)


  ### aurum ----

  dbName_aurum <- "aurum_pc"
  con_aurum <- dbConnect(
    drv = Postgres(),
    dbname = "cdm_aurum_p22_001867",
    host = host,
    port = port,
    user = username,
    password = password
  )


  cdm_a <- cdmFromCon(
    con = con_aurum, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_aurum,
    cohortTables = c(cohort_name, cohort_name_long, cohort_name_visits, t_status_cohort, n_status_cohort, psa_cohort, gleason_cohort, "progression", "medications", "conditions", "type2_diabetes")
  )


  cdm_a$observation_period <- cdm_a$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)


  ### merged cohort ----

  cohort_merged <- dplyr::bind_rows(
    cdm_g[[cohort_name_long]] |>
      dplyr::mutate("source" = "gold") |>
      PatientProfiles::addAge() |>
      dplyr::collect(),
    cdm_a[[cohort_name_long]] |>
      dplyr::mutate(source = "aurum") |>
      PatientProfiles::addAge() |>
      dplyr::collect()
  )

  frequent_concepts <- getFrequentConcepts(cohort = cohort_merged)


  visits <- cdm_a[[cohort_name_visits]] |>
    dplyr::collect() |>
    dplyr::bind_rows(cdm_g[[cohort_name_visits]] |>
      dplyr::collect())

  wide_data <- getWideData(cohort = cohort_merged, frequent_concepts = frequent_concepts, visits = visits)

  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 0, 1))

  ### Lasso ----

  x <- getSelectedFeatures(
    wide_data = wide_data,
    cdm = cdm_a,
    cdm_name = "merged"
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
      cdm_name = "merged",
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
      group = "cohort", strata = "variable",
      additional = c("concept_name", "domain_id", "window"),
      estimates = "coefficient", settings = "result_type"
    ) |>
    dplyr::mutate(cdm_name = "merged")

  asmd <- computeASMD(wide_data = wide_data, features = x$selected_columns)

  result[["asmd"]] <- asmd |>
    tidyr::pivot_longer(
      cols = c(smd, asmd),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      cohort = cohort_name,
      cdm_name = "merged",
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
    cdm_name = "merged"
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
      cdm_name = "merged",
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
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pair_id", "source", dplyr::starts_with("latest"))


  gold_matched <- matched_data |>
    dplyr::filter(.data$source == "gold") |>
    dplyr::select(!"source")

  aurum_matched <- matched_data |>
    dplyr::filter(.data$source == "aurum") |>
    dplyr::select(!"source")

  merged_matched_cohort_name <- paste("merged", cohort_name_matched, sep = "_")

  cdm_g <- omopgenerics::insertTable(cdm = cdm_g, name = merged_matched_cohort_name, table = gold_matched)

  cdm_g[[merged_matched_cohort_name]] <- omopgenerics::newCohortTable(table = cdm_g[[merged_matched_cohort_name]])

  cdm_a <- omopgenerics::insertTable(cdm = cdm_a, name = merged_matched_cohort_name, table = aurum_matched)

  cdm_a[[merged_matched_cohort_name]] <- omopgenerics::newCohortTable(table = cdm_a[[merged_matched_cohort_name]])


  result[["characterisation_matched_cohort"]] <- mergedCohortCharacterisation(cdm_g = cdm_g, cdm_a = cdm_a, cohort_name = merged_matched_cohort_name)


  ### Outcome model ----

  covariates <- NULL
  outcome_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "Outcomes"), type = "csv")
  outcomes <- clean_names(names(outcome_codelist))
  names(outcome_codelist) <- outcomes

  death_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "CauseOfDeath"), type = "csv")
  death_pc_codes <- death_codelist[["prostate_cancer_death"]]
  death_cvd_codes <- death_codelist[["cvd_death"]]


  survival_gold <- cdm_g[[merged_matched_cohort_name]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
    dplyr::mutate(death = dplyr::coalesce(.data[["death"]], 999999L)) |>
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
    dplyr::collect()


  survival_aurum <- cdm_a[[merged_matched_cohort_name]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
    dplyr::mutate(death = dplyr::coalesce(.data[["death"]], 999999L)) |>
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
    dplyr::collect()


  for (out in c(outcomes, "type2_diabetes")) {
    washout_name <- paste0(out, "_washout")
    window <- list(c(1, Inf))
    if (out == "androgen_deprivation") {
      window <- list(c(181, Inf))
    } else {
      window <- list(c(1, Inf))
    }
    if (out != "type2_diabetes") {
    cdm_g[[merged_matched_cohort_name]] <- cdm_g[[merged_matched_cohort_name]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = window,
        nameStyle = out,
        name = merged_matched_cohort_name
      ) |>
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = list(c(-365, 0)),
        targetStartDate = "event_end_date",
        name = merged_matched_cohort_name,
        nameStyle = washout_name,
      ) |>
      dplyr::mutate(
        !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
      )
    cdm_a[[merged_matched_cohort_name]] <- cdm_a[[merged_matched_cohort_name]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = window,
        nameStyle = out,
        name = merged_matched_cohort_name
      ) |>
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = list(out = outcome_codelist[[out]]),
        window = list(c(-365, 0)),
        targetStartDate = "event_end_date",
        name = merged_matched_cohort_name,
        nameStyle = washout_name,
      ) |>
      dplyr::mutate(
        !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
      )

    } else {
      cdm_g[[merged_matched_cohort_name]] <- cdm_g[[merged_matched_cohort_name]] |>
        PatientProfiles::addCohortIntersectDays(
          targetCohortTable = "type2_diabetes",
          window = window,
          nameStyle = "type2_diabetes",
          name = merged_matched_cohort_name
        ) |>
        PatientProfiles::addCohortIntersectFlag(
          targetCohortTable = "type2_diabetes",
          window = list(c(-365, 0)),
          targetStartDate = "cohort_end_date",
          name = merged_matched_cohort_name,
          nameStyle = washout_name
        ) |>
        dplyr::mutate(
          !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
        )
      cdm_a[[merged_matched_cohort_name]] <- cdm_a[[merged_matched_cohort_name]] |>
        PatientProfiles::addCohortIntersectDays(
          targetCohortTable = "type2_diabetes",
          window = window,
          nameStyle = "type2_diabetes",
          name = merged_matched_cohort_name
        ) |>
        PatientProfiles::addCohortIntersectFlag(
          targetCohortTable = "type2_diabetes",
          window = list(c(-365, 0)),
          targetStartDate = "cohort_end_date",
          name = merged_matched_cohort_name,
          nameStyle = washout_name
        ) |>
        dplyr::mutate(
          !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
        )
    }

    pairs <- cdm_g[[merged_matched_cohort_name]] |>
      dplyr::filter(.data[[washout_name]] == 1L) |>
      dplyr::pull(.data$pair_id)

    cdm_g[[merged_matched_cohort_name]] <- cdm_g[[merged_matched_cohort_name]] |>
      dplyr::mutate(!!rlang::sym(out) := dplyr::if_else(.data$pair_id %in% pairs, 999999L, .data[[out]])) |>
      dplyr::select(!dplyr::all_of(washout_name)) |>
      dplyr::compute(merged_matched_cohort_name)

    survival_gold <- survival_gold |>
      dplyr::left_join(cdm_g[[merged_matched_cohort_name]] |>
        dplyr::collect())

    pairs <- cdm_a[[merged_matched_cohort_name]] |>
      dplyr::filter(.data[[washout_name]] == 1L) |>
      dplyr::pull(.data$pair_id)

    cdm_a[[merged_matched_cohort_name]] <- cdm_a[[merged_matched_cohort_name]] |>
      dplyr::mutate(!!rlang::sym(out) := dplyr::if_else(.data$pair_id %in% pairs, 999999L, .data[[out]])) |>
      dplyr::select(!dplyr::all_of(washout_name)) |>
      dplyr::compute(merged_matched_cohort_name)

    survival_aurum <- survival_aurum |>
      dplyr::left_join(cdm_a[[merged_matched_cohort_name]] |>
        dplyr::collect())
  }

  survival_data <- survival_gold |>
    dplyr::mutate(source = "gold") |>
    dplyr::bind_rows(survival_aurum |>
      dplyr::mutate(source = "aurum"))

  outcomes <- c("death", "death_cvd", "death_pc", outcomes)

  res_outcomes <- outcomes |>
    purrr::map(\(out) {
      outcomeModel(survival_data = survival_data, outcome = out)
    })
  result[["survival"]] <- res_outcomes |>
    bindResults(cdmName = "merged", cohort_name = cohort_name)
### NCO ----

  nco_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "NCO"), type = "csv")
  negative_control_outcomes <- clean_names(names(nco_codelist))
  names(nco_codelist) <- negative_control_outcomes



  nco_gold <- cdm_g[[merged_matched_cohort_name]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
    dplyr::mutate(death = dplyr::coalesce(.data[["death"]], 999999L)) |>
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
    dplyr::collect()


  nco_aurum <- cdm_a[[merged_matched_cohort_name]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
    dplyr::mutate(death = dplyr::coalesce(.data[["death"]], 999999L)) |>
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
    dplyr::collect()


  for (nco in negative_control_outcomes) {

    window <- list(c(1, Inf))

    cdm_g[[merged_matched_cohort_name]] <- cdm_g[[merged_matched_cohort_name]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(nco = nco_codelist[[nco]]),
        window = window,
        nameStyle = nco,
        name = merged_matched_cohort_name
      )



    nco_gold <- nco_gold |>
      dplyr::left_join(cdm_g[[merged_matched_cohort_name]] |>
                         dplyr::collect())


    cdm_a[[merged_matched_cohort_name]] <- cdm_a[[merged_matched_cohort_name]] |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(nco = nco_codelist[[nco]]),
        window = window,
        nameStyle = nco,
        name = merged_matched_cohort_name
      )

    nco_aurum <- nco_aurum |>
      dplyr::left_join(cdm_a[[merged_matched_cohort_name]] |>
                         dplyr::collect())
  }

  nco_table <- nco_gold |>
    dplyr::mutate(source = "gold") |>
    dplyr::bind_rows(nco_aurum |>
                       dplyr::mutate(source = "aurum"))

  res_nco <- negative_control_outcomes |>
    purrr::map(\(nco) {
      NCOModel(survival_data = nco_table, outcome = nco)
    })
  result[["nco"]] <- res_nco |>
    bindResults(cdmName = "merged", cohort_name = cohort_name)
  set <- omopgenerics::settings(result[["nco"]]) |>
    dplyr::mutate(result_type = paste0("nco_", .data$result_type))

  result[["nco"]] <- omopgenerics::newSummarisedResult(result[["nco"]], settings = set)



  ### Export ----
  result_to_export <- omopgenerics::bind(result)
  omopgenerics::exportSummarisedResult(result, path = output_directory, fileName = paste0("results_{cdm_name}_", cohort_name, "_{date}.csv"))
}
CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
