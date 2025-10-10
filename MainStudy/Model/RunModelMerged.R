host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)



source("MainStudy/Model/functions.R")

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |> unlist() |> unname()
output_directory = here::here("MainStudy/Results")

cohort_name = "optima_pc_trial"
cohort_name_long <- paste(cohort_name, "long", sep = "_")
cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
cohort_name_matched <- paste(cohort_name, "matched", sep = "_")


result <- list()

### gold ----
dbName_gold <-"gold_pc"
con_gold <- dbConnect(drv = Postgres(),
                      dbname = "cdm_gold_p22_001867",
                      host = host,
                      port = port,
                      user = username,
                      password = password)


cdm_g <- cdmFromCon(con = con_gold, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_gold,
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_long", "optima_pc_rwd_visits","optima_pc_trial_long", "optima_pc_trial_visits", "progression"))


cdm_g$observation_period <- cdm_g$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)


### aurum ----

dbName_aurum <-"aurum_pc"
con_aurum <- dbConnect(drv = Postgres(),
                       dbname = "cdm_aurum_p22_001867",
                       host = host,
                       port = port,
                       user = username,
                       password = password)


cdm_a <- cdmFromCon(con = con_aurum, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_aurum,
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_long", "optima_pc_rwd_visits","optima_pc_trial_long", "optima_pc_trial_visits", "progression"))


cdm_a$observation_period <- cdm_a$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)


### merged cohort ----

cohort_rwd <- dplyr::bind_rows(cdm_g[[cohort_name_long]] |>
                                 dplyr::mutate("source" = "gold") |>
                                 PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf))) |>
                                 dplyr::collect(),
                               cdm_a[[cohort_name_long]] |>
                                 dplyr::mutate(source = "aurum") |>
                                 PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf))) |>
                                 dplyr::collect()
)

frequent_concepts <- getFrequentConcepts(cohort = cohort_rwd, excluded_codes = excluded_codes)


visits <- cdm_a[[cohort_name_visits]] |>
  dplyr::collect() |>
  dplyr::bind_rows(cdm_g[[cohort_name_visits]] |>
                     dplyr::collect())

wide_data <- getWideData(cohort = cohort_rwd, frequent_concepts, visits)

wide_data <- wide_data |>
  dplyr::distinct() |>
  dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0),
  )

### Lasso ----

x <- getSelectedFeatures(wide_data = wide_data,
                         directory = paste0(output_directory, "/Lasso"),
                         cdm = cdm_a,
                         cdm_name = "merged")



result[["density_points"]] <- x$density_points |>
  dplyr::mutate(
    strata_name  = "treatment",
    strata_level = as.character(.data$treatment),
    idx          = dplyr::row_number(),
    density_x    = .data$x,
    density_y    = .data$y
  ) |>
  dplyr::mutate(variable_level = paste("density", .data$idx, sep ="_"))|>
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
    variable_name  = "Propensity score distribution",
    result_id = 1L
  ) |>
  dplyr::select(!c("treatment", "x", "y", "idx")) |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  omopgenerics::uniteAdditional() |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "distribution_ps"))

result[["selected_features"]] <- x$selected_features |>
  dplyr::mutate(coefficient = sprintf("%.4f", .data$coefficient),
                cohort = cohort_name,
                result_type = "selected_features",
                variable_name = "event X=1",
                variable_level = .data$event) |>
  omopgenerics::transformToSummarisedResult(group = "cohort", strata = "variable",
                                            additional = c("concept_name","domain_id"),
                                            estimates = "coefficient", settings = "result_type") |>
  dplyr::mutate(cdm_name = "merged")

asmd <- computeASMD(wide_data = wide_data, features = x$selected_columns)

result[["asmd"]] <- asmd |>
  tidyr::pivot_longer(
    cols = c(smd, asmd),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(cohort = cohort_name,
                cdm_name = "merged",
                estimate_value = sprintf("%.3f", .data$estimate_value),
                estimate_type = "numeric",
                variable_name = "covariate",
                variable_level = .data$covariate,
                result_id = 1L) |>
  omopgenerics::uniteAdditional(cols = c("event", "comparator")) |>
  omopgenerics::uniteStrata() |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  dplyr::select(!"covariate") |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))


### Matching ----

matched_data <- getMatchedData(selectedFeatures = x$selected_columns,
                               wide_data = wide_data,
                               directory = paste0(output_directory, "/Matching"),
                               cdm_name = "merged")

asmd_matched <- computeASMD(wide_data = matched_data, features = x$selected_columns)

result[["asmd_matched"]] <- asmd_matched |>
  tidyr::pivot_longer(
    cols = c(smd, asmd),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(cohort = cohort_name_matched,
                cdm_name = "merged",
                estimate_value = sprintf("%.3f", .data$estimate_value),
                estimate_type = "numeric",
                variable_name = "covariate",
                variable_level = .data$covariate,
                result_id = 1L) |>
  omopgenerics::uniteAdditional(cols = c("event", "comparator")) |>
  omopgenerics::uniteStrata() |>
  omopgenerics::uniteGroup(cols = "cohort") |>
  dplyr::select(!"covariate") |>
  omopgenerics::newSummarisedResult(settings = tibble::tibble(result_id = 1L, result_type = "asmd"))

matched_data <- matched_data |>
  dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",  "cohort_end_date", "pair_id", "source")


gold_matched <- matched_data |>
  dplyr::filter(.data$source == "gold")

aurum_matched <- matched_data |>
  dplyr::filter(.data$source == "aurum")

marged_matched_cohort_name <- paste("merged", cohort_name_matched, sep = "_")

cdm_g <- omopgenerics::insertTable(cdm = cdm_g, name = marged_matched_cohort_name, table = gold_matched)

cdm_g[[marged_matched_cohort_name]] <- omopgenerics::newCohortTable(table = cdm_g[[marged_matched_cohort_name]])

cdm_a <- omopgenerics::insertTable(cdm = cdm_a, name = marged_matched_cohort_name, table = aurum_matched)

cdm_a[[marged_matched_cohort_name]] <- omopgenerics::newCohortTable(table = cdm_a[[marged_matched_cohort_name]])





### Outcome model ----

covariates = NULL

outcome_codelist <- omopgenerics::importCodelist(here::here("Codelist/Outcomes"), type = "csv")
outcomes <- clean_names(names(outcome_codelist))
names(outcome_codelist) <- outcomes



survival_gold <- cdm_g[[marged_matched_cohort_name]] |>
  PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
  PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
  PatientProfiles::addCohortIntersectDays(targetCohortTable = "progression", nameStyle = "progression", window = c(1, Inf)) |>
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
  dplyr::collect()


survival_aurum <- cdm_a[[marged_matched_cohort_name]] |>
  PatientProfiles::addDeathDays(deathDaysName = "death",window = c(1, Inf)) |>
  PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
  PatientProfiles::addCohortIntersectDays(targetCohortTable = "progression", nameStyle = "progression", window = c(1, Inf) ) |>
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
  dplyr::collect()



for (out in outcomes){

  washout_name <-paste0(out, "_washout")

  cdm_g[[marged_matched_cohort_name]] <- cdm_g[[marged_matched_cohort_name]] |>
    PatientProfiles::addConceptIntersectDays(conceptSet = list(out = outcome_codelist[[out]]),
                                             window = list(c(1, Inf)),
                                             nameStyle = out,
                                             name = marged_matched_cohort_name) |>
    PatientProfiles::addConceptIntersectFlag(conceptSet = list(out = outcome_codelist[[out]]),
                                             window = list(c(-365, 0)),
                                             targetStartDate = "event_end_date",
                                             name = marged_matched_cohort_name,
                                             nameStyle = washout_name, ) |>

    dplyr::mutate(
      !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
    )

  pairs <- cdm_g[[marged_matched_cohort_name]] |>
    dplyr::filter(.data[[washout_name]] == 1L) |>
    dplyr::pull(.data$pair_id)

  cdm_g[[marged_matched_cohort_name]] <- cdm_g[[marged_matched_cohort_name]] |>
    dplyr::mutate( !!rlang::sym(out) := dplyr::if_else(.data$pair_id %in% pairs,  999999L ,.data[[out]])) |>
    dplyr::select(!dplyr::all_of(washout_name)) |>
    dplyr::compute(marged_matched_cohort_name)



  survival_gold <- survival_gold |>
    dplyr::left_join( cdm_g[[marged_matched_cohort_name]] |>
                        dplyr::collect()
    )


  cdm_a[[marged_matched_cohort_name]] <- cdm_a[[marged_matched_cohort_name]] |>
    PatientProfiles::addConceptIntersectDays(conceptSet = list(out = outcome_codelist[[out]]),
                                             window = list(c(1, Inf)),
                                             nameStyle = out,
                                             name = marged_matched_cohort_name) |>
    PatientProfiles::addConceptIntersectFlag(conceptSet = list(out = outcome_codelist[[out]]),
                                             window = list(c(-365, 0)),
                                             targetStartDate = "event_end_date",
                                             name = marged_matched_cohort_name,
                                             nameStyle = washout_name, ) |>

    dplyr::mutate(
      !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
    )

  pairs <- cdm_a[[marged_matched_cohort_name]] |>
    dplyr::filter(.data[[washout_name]] == 1L) |>
    dplyr::pull(.data$pair_id)

  cdm_a[[marged_matched_cohort_name]] <- cdm_a[[marged_matched_cohort_name]] |>
    dplyr::mutate( !!rlang::sym(out) := dplyr::if_else(.data$pair_id %in% pairs,  999999L ,.data[[out]])) |>
    dplyr::select(!dplyr::all_of(washout_name)) |>
    dplyr::compute(marged_matched_cohort_name)



  survival_aurum <- survival_aurum |>
    dplyr::left_join( cdm_a[[marged_matched_cohort_name]] |>
                        dplyr::collect()
    )


}

survival_data <-  survival_gold |>
  dplyr::mutate(source = "gold") |>
  dplyr::bind_rows(survival_aurum |>
                     dplyr::mutate(source = "aurum")
  )
outcomes <- c("death", "progression", outcomes)

res_outcomes <- outcomes |>
  purrr::map(\(out) {
    outcomeModel(survival_data = survival_data, outcome = out)
  })
result[["survival"]] <- res_outcomes |>
  bindResults(cdmName = "merged", cohort_name = cohort_name)

result_to_export <- omopgenerics::bind(result)
omopgenerics::exportSummarisedResult(result, path = paste0(output_directory, "/Survival"), fileName = paste0("results_{cdm_name}_", cohort_name,".csv"))


CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
