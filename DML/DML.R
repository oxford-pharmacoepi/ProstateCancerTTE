host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

library(dplyr)

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

excluded_codes <- omopgenerics::importCodelist(path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv") |> unlist() |> unname()

### gold ----


n_gold <- cdm_g$optima_pc_rwd |>
  dplyr::distinct(subject_id) |>
  dplyr::tally() |>
  dplyr::pull()|>
  as.numeric()

cdm_g$optima_pc_rwd_long <- cdm_g$optima_pc_rwd |>
  dplyr::left_join(cdm_g$condition_occurrence |> dplyr::select(person_id, condition_concept_id, condition_start_date), by = c("subject_id" = "person_id")) |>
  dplyr::filter(.data$condition_start_date < .data$cohort_start_date) |>
  dplyr::rename("concept_id" = "condition_concept_id")|>
  dplyr::select(!c(condition_start_date))|>
  dplyr::union_all( cdm_g$optima_pc_rwd |>
                      dplyr::left_join(cdm_g$drug_exposure |> dplyr::select(person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date), by = c("subject_id" = "person_id")) |>
                      dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$drug_exposure_start_date),
                                    days_diff_end = as.integer(.data$cohort_start_date - .data$drug_exposure_end_date)) |>
                      dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start > 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end > 0 ) | (.data$cohort_start_date > .data$drug_exposure_start_date & .data$cohort_start_date < .data$drug_exposure_end_date)) |>
                      dplyr::rename("concept_id" = "drug_concept_id") |>
                      dplyr::select(!c(drug_exposure_start_date, drug_exposure_end_date, days_diff_start, days_diff_end))) |>
  dplyr::compute(name = "optima_pc_rwd_long")

frequent_concepts_gold <- cdm_g$optima_pc_rwd_long |>
  dplyr::filter(!(.data$concept_id %in% excluded_codes))|>
  dplyr::group_by(concept_id) |>
  dplyr::summarise(n_subjects = (dplyr::n_distinct(subject_id)), .groups = "drop") |>
  dplyr::mutate(perc = .data$n_subjects / n_gold) |>
  dplyr::filter(perc >= 0.005)|> dplyr::pull(concept_id)

cdm_g$optima_pc_rwd_visits <- cdm_g$optima_pc_rwd |>
  dplyr::left_join(cdm_g$visit_occurrence |> dplyr::filter(.data$visit_concept_id != 38004268) |> dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date), by = c("subject_id" = "person_id")) |>
  dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$visit_start_date),
                days_diff_end = as.integer(.data$cohort_start_date - .data$visit_end_date)) |>
  dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start >= 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end >= 0 ) | (.data$cohort_start_date > .data$visit_start_date & .data$cohort_start_date < .data$visit_end_date)) |>
  dplyr::group_by(subject_id, visit_concept_id, cohort_definition_id, cohort_start_date, cohort_end_date) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = "visit_concept_id", values_from = "n", names_prefix = "visit_", values_fill = list(n = 0)) |>

  dplyr::compute(name = "optima_pc_rwd_visits")



wide_data_gold <- cdm_g$optima_pc_rwd_long |> dplyr::filter(concept_id %in% frequent_concepts_gold) |>
  PatientProfiles::addAgeQuery() |>
  dplyr::mutate(value = 1) |>
  tidyr::pivot_wider(
    names_from = concept_id,
    values_from = value,
    values_fill = list(value = 0),
  ) |>
  dplyr::left_join(cdm_g$optima_pc_rwd_visits, by = c("subject_id", "cohort_definition_id", "cohort_start_date", "cohort_end_date")) |>
  dplyr::collect() |>
  dplyr::mutate(year = clock::get_year(cohort_start_date),
                visit_9201 = dplyr::coalesce(visit_9201, 0),
                visit_581477 = dplyr::coalesce(visit_581477, 0))

wide_data_gold <- wide_data_gold |>
  dplyr::distinct() |>
  dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0),
  )
y <- wide_data_gold$y
X <- wide_data_gold |>
  dplyr::select(-c("y", "value_as_number"))
X <- stats::model.matrix(
  ~ . - cohort_definition_id - cohort_start_date - cohort_end_date - subject_id + age + year ,
  data = X
)[, -1]

lasso_fit <- glmnet::cv.glmnet(
  x = X,
  y = y,
  family = "binomial",
  alpha = 1
)

# Raw residuals for dml (for binary Y)
residuals_y <- y - as.numeric(y_hat)




CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
