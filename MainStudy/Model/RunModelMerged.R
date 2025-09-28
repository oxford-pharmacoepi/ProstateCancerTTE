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


### gold ----
dbName_gold <-"gold_pc"
con_gold <- dbConnect(drv = Postgres(),
                      dbname = "cdm_gold_p22_001867",
                      host = host,
                      port = port,
                      user = username,
                      password = password)


cdm_g <- cdmFromCon(con = con_gold, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_gold,
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_long", "optima_pc_rwd_visits", "progression"))


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
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_long", "optima_pc_rwd_visits", "progression"))


cdm_a$observation_period <- cdm_a$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)


### all ----

cohort_rwd <- dplyr::bind_rows(cdm_g$optima_pc_rwd_long |>
                                 dplyr::mutate("source" = "gold") |>
                                 PatientProfiles::addAge() |>
                                 dplyr::collect(),
                               cdm_a$optima_pc_rwd_long |>
                                 dplyr::mutate(source = "aurum") |>
                                 PatientProfiles::addAge() |>
                                 dplyr::collect()
                                 )

frequent_concepts <- getFrequentConcepts(cohort = cohort_rwd, excluded_codes = excluded_codes)


visits <- cdm_a$optima_pc_rwd_visits |>
  dplyr::collect() |>
  dplyr::bind_rows(cdm_g$optima_pc_rwd_visits |>
                     dplyr::collect())

wide_data <- getWideData(cohort = cohort_rwd, frequent_concepts, visits)

selectedFeatures <- getSelectedFeatures(wide_data = wide_data,
                                        directory = paste0(output_directory, "/Lasso"),
                                        cdm = cdm_a,
                                        cdm_name = "merged")


matched_data <- getMatchedData(selectedFeatures = selectedFeatures,
                               wide_data = wide_data,
                               directory = paste0(output_directory, "/Matching"),
                               cdm_name = "merged")


gold_matched_subjects <- matched_data |>
  dplyr::filter(.data$source == "gold") |>
  dplyr::pull(.data$subject_id)

aurum_matched_subjects <- matched_data |>
  dplyr::filter(.data$source == "aurum") |>
  dplyr::pull(.data$subject_id)


matched_data <- matched_data |>
  dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",  "cohort_end_date")


cdm_g$merged_matched_data <- cdm_g$optima_pc_rwd |>
  dplyr::filter(.data$subject_id %in% gold_matched_subjects) |>
  dplyr::compute(name = "merged_matched_data") |>
  omopgenerics::newCohortTable()

cdm_a$merged_matched_data <- cdm_a$optima_pc_rwd |>
  dplyr::filter(.data$subject_id %in% aurum_matched_subjects) |>
  dplyr::compute(name = "merged_matched_data")


### Outcome model ----

covariates = NULL

outcome_codelist <- omopgenerics::importCodelist(here::here("Codelist/Outcomes"), type = "csv")
outcomes <- clean_names(names(outcome_codelist))
names(outcome_codelist) <- outcomes

cdm_g[["merged_survival_data"]] <- cdm_g[["merged_matched_data"]] |>
  PatientProfiles::addDeathDays(deathDaysName = "death", name = "merged_survival_data") |>
  PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation", name = "merged_survival_data") |>
  PatientProfiles::addCohortIntersectDays(targetCohortTable = "progression", nameStyle = "progression", name = "merged_survival_data" ) |>
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
  dplyr::compute(name = "merged_survival_data")


cdm_a[["merged_survival_data"]] <- cdm_a[["merged_matched_data"]] |>
  PatientProfiles::addDeathDays(deathDaysName = "death", name = "merged_survival_data") |>
  PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation", name = "merged_survival_data") |>
  PatientProfiles::addCohortIntersectDays(targetCohortTable = "progression", nameStyle = "progression", name = "merged_survival_data" ) |>
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
  dplyr::compute(name = "merged_survival_data")



for (out in outcomes){

  cdm_g[["merged_survival_data"]] <- cdm_g[["merged_survival_data"]] |>
    PatientProfiles::addConceptIntersectDays(conceptSet = list(out = outcome_codelist[[out]]),
                                             nameStyle = out,
                                             name = "merged_survival_data") |>
    dplyr::mutate(
      !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
    )
  cdm_a[["merged_survival_data"]] <- cdm_a[["merged_survival_data"]] |>
    PatientProfiles::addConceptIntersectDays(conceptSet = list(out = outcome_codelist[[out]]),
                                             nameStyle = out,
                                             name = "merged_survival_data") |>
    dplyr::mutate(
      !!rlang::sym(out) := dplyr::coalesce(.data[[out]], 999999L)
    )
}

survival_data <-  cdm_g[["merged_survival_data"]] |>
  dplyr::mutate(source = "gold") |>
  dplyr::collect() |>
  dplyr::bind_rows(cdm_a[["merged_survival_data"]] |>
                     dplyr::mutate(source = "aurum") |>
                     dplyr::collect())
outcomes <- c("death", "progression", outcomes)
result <- outcomes |>
  purrr::map(\(out) {
    outcomeModel(survival_data = survival_data, outcome = out)
  }) |>
  bindResults(cdmName = "merged")

omopgenerics::exportSummarisedResult(result, path = paste0(output_directory, "/Survival"), fileName = "survival_results_{cdm_name}.csv")


CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
