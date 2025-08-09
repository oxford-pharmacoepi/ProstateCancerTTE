host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

library(dplyr)



source("MainStudy/PropensityScores/functions.R")
excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |> unlist() |> unname()


### gold ----

dbName_gold <-"gold_pc"
con_gold <- dbConnect(drv = Postgres(),
                      dbname = "cdm_gold_p22_001867",
                      host = host,
                      port = port,
                      user = username,
                      password = password)


cdm_g <- cdmFromCon(con = con_gold, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_gold,
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd"))


cdm_g$observation_period <- cdm_g$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)

cdm_g <- addVariables(cdm_g)
cdm_g <- longDataFromCohort(cdm_g)

frequent_concepts_gold <- getFrequentConcepts(cohort = cdm_g$optima_pc_rwd_long, excluded_codes = excluded_codes)

cdm_g <- visitsCount(cdm_g)
cdm_g$optima_pc_rwd_long <- cdm_g$optima_pc_rwd_long |>
  PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf)))

wide_data_gold <- getWideData(cdm_g$optima_pc_rwd_long, frequent_concepts_gold, cdm_g$optima_pc_rwd_visits)

selectedFeatures_gold <- getSelectedFeatures(wide_data = wide_data_gold,
                                             directory = here::here("MainStudy/Results/Lasso/Gold"),
                                             cdm = cdm_g)
selectedFeatures_gold

matching_gold <- getMatchedData(selectedFeatures = selectedFeatures_gold,
                                wide_data = wide_data_gold,
                                directory = here::here("MainStudy/Results/Matching/Gold"))



### aurum ----

dbName_aurum <-"aurum_pc"
con_aurum <- dbConnect(drv = Postgres(),
                       dbname = "cdm_aurum_p22_001867",
                       host = host,
                       port = port,
                       user = username,
                       password = password)


cdm_a <- cdmFromCon(con = con_aurum, cdmSchema = "public", writeSchema = "results", achillesSchema = "results", writePrefix = "cc_", .softValidation = TRUE, cdmName = dbName_aurum,
                    cohortTables = c("optima_pc_trial", "optima_pc_rwd"))


cdm_a$observation_period <- cdm_a$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)

cdm_a <- addVariables(cdm_a)
cdm_a <- longDataFromCohort(cdm_a)

frequent_concepts_aurum <- getFrequentConcepts(cohort = cdm_a$optima_pc_rwd_long, excluded_codes = excluded_codes)

cdm_a <- visitsCount(cdm_a)
cdm_a$optima_pc_rwd_long <- cdm_a$optima_pc_rwd_long |>
  PatientProfiles::addAgeQuery(ageGroup = list(c(0,50), c(51,55), c(56, 60), c(61,65), c(66,70), c(71, 75), c(76,80), c(81, Inf)))

wide_data_aurum <- getWideData(cdm_a$optima_pc_rwd_long, frequent_concepts_aurum, cdm_a$optima_pc_rwd_visits)

selectedFeatures_aurum <- getSelectedFeatures(wide_data = wide_data_aurum,
                                             directory = here::here("MainStudy/Results/Lasso/Aurum"),
                                             cdm = cdm_a)


matching_aurum <- getMatchedData(selectedFeatures = selectedFeatures_aurum,
                                wide_data = wide_data_aurum,
                                directory = here::here("MainStudy/Results/Matching/Aurum"))



### all ----

cohort_rwd <- dplyr::bind_rows(cdm_a$optima_pc_rwd_long |>
                                 dplyr::mutate("source" = "gold") |>
                                 dplyr::collect(),
                               cdm_g$optima_pc_rwd_long |>
                                 dplyr::mutate(source = "aurum") |>
                                 dplyr::collect())

frequent_concepts <- getFrequentConcepts(cohort = cohort_rwd, excluded_codes = excluded_codes)


visits <- cdm_a$optima_pc_rwd_visits |>
  dplyr::collect() |>
  dplyr::bind_rows(cdm_g$optima_pc_rwd_visits |>
                     dplyr::collect())

wide_data <- getWideData(cohort = cohort_rwd, frequent_concepts, visits)

selectedFeatures <- getSelectedFeatures(wide_data = wide_data,
                                              directory = here::here("MainStudy/Results/Lasso/Merged"),
                                              cdm = cdm_a)


matching_merged <- getMatchedData(selectedFeatures = selectedFeatures,
                                 wide_data = wide_data,
                                 directory = here::here("MainStudy/Results/Matching/Merged"))




CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
