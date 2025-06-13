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


excluded_codes <- prostate_items <- list(
  `Hormone therapy` = 4061650,
  `Seen in oncology clinic` = 4083427,
  `Implied consent for core Summary Care Record dataset upload` = 44805213,
  `Transurethral biopsy of prostate` = 4071781,
  `Primary malignant neoplasm of prostate` = 200962,
  `Depression screening using questions` = 44788755,
  `Informed consent for procedure` = 4082261,
  `Preparation for intensity modulated radiation therapy` = 44793209,
  `Injection` = 4241075,
  `Choice and booking enhanced services administration` = 44790074,
  `goserelin 3.6 MG Drug Implant [Zoladex] by Astrazeneca` = 36935860,
  `Echography of prostate, transrectal approach` = 4317528,
  `Leuprolide 11.3 MG Injectable Suspension [Prostap] by Takeda` = 35774257,
  `Subcutaneous injection of hormone antagonist` = 4193579,
  `bicalutamide 50 MG Oral Tablet` = 1344403,
  `Fast track referral for suspected urological cancer` = 44792638,
  `Goserelin 10.8 MG Drug Implant [Zoladex LA] by Astrazeneca` = 21089544,
  `Goserelin 3.6 MG Drug Implant [Zoladex] by Astrazeneca` = 21128764,
  `Injection into subcutaneous tissue` = 37151637,
  `Discharged from hospital` = 4084843,
  `Seen in urology clinic` = 4083414,
  `Number of neoplasms in excised tissue specimen` = 4078513,
  `Transperineal needle biopsy of prostate` = 4142388
)


cohort_rwd <- cdm_a$optima_pc_rwd |> PatientProfiles::addAge()|> dplyr::mutate("source" = "Aurum") |>
  dplyr::collect()  |>
  dplyr::bind_rows(cdm_g$optima_pc_rwd |> PatientProfiles::addAge()  |> dplyr::mutate("source" = "Gold") |> dplyr::collect()) |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup()

conditions <- cdm_a$condition_occurrence |>
  dplyr::select(person_id, condition_concept_id, condition_start_date, condition_end_date)|>
  dplyr::filter(person_id %in% cohort_rwd$subject_id) |>
  dplyr::collect() |>
  dplyr::bind_rows(cdm_g$condition_occurrence |>
                     dplyr::select(person_id, condition_concept_id, condition_start_date, condition_end_date)|>
                     dplyr::filter(person_id %in% cohort_rwd$subject_id) |>
                     dplyr::collect()) |>
  dplyr::filter(!(.data$condition_concept_id %in% unname(unlist(excluded_codes))))


cohort_rwd <- cohort_rwd |> dplyr::left_join(conditions |> dplyr::select(person_id, condition_concept_id, condition_start_date, condition_end_date), by = c("subject_id" = "person_id")) |>
  dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$condition_start_date),
                days_diff_end = as.integer(.data$cohort_start_date - .data$condition_end_date)
                ) |>
  dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start >= 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end >= 0 ) | (.data$cohort_start_date > .data$condition_start_date & .data$cohort_start_date < .data$condition_end_date)) |>
  dplyr::mutate(
    year = clock::get_year(cohort_start_date),
    year_group = dplyr::case_when(
      year < 2010 ~ "<2010",
      year >= 2010 & year < 2015 ~ "2010-2014",
      year >= 2015 & year < 2020 ~ "2015-2019",
      year >= 2020  ~ ">2020",
    )
  )


n_subjects <- cohort_rwd |> dplyr::group_by(source) |> dplyr::summarise(n = n_distinct(subject_id))

frequent_conditions <- cohort_rwd |>
  dplyr::left_join(n_subjects, by = "source")|>
  dplyr::group_by(dplyr::across(c(source, condition_concept_id))) |>
  dplyr::reframe(n = dplyr::n_distinct( subject_id) / n) |>
  dplyr::filter(n > 0.005 )
cohort_filtered <- cohort_rwd |>
  dplyr::semi_join(frequent_conditions, by = "condition_concept_id")


wide_data <- cohort_filtered |>
  dplyr::distinct(subject_id, cohort_definition_id, age, source, year_group, value_as_number, condition_concept_id) |>
  dplyr::mutate(condition = paste0("cond_", condition_concept_id)) |>
  dplyr::mutate(value = 1) |>
  tidyr::pivot_wider(
    names_from = condition,
    values_from = value,
    values_fill = 0
  ) |>
  dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0)) |>
  dplyr::ungroup()

y <- wide_data$y
X <- wide_data |>
  dplyr::select(- c(condition_concept_id, subject_id, cohort_definition_id, y, value_as_number))
X <- stats::model.matrix(
  ~ age + source + year_group  + .,
  data = X
)[, -1]
lasso_fit <- glmnet::cv.glmnet(
  x = X,
  y = y,
  family = "binomial",
  alpha = 1
)


coefs <- glmnet::coef.glmnet(lasso_fit, s = "lambda.1se")
selectedLassoFeatures <- names(coefs[(coefs[,1]!=0),1])
selectedLassoFeatures <- selectedLassoFeatures[selectedLassoFeatures!="(Intercept)"]
propensity_scores <- stats::predict(lasso_fit, newx = X, s = "lambda.1se", type = "response")[, 1]
plot_data <- dplyr::tibble(
  propensity_score = propensity_scores,
  treatment = base::as.factor(y)
)
p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = propensity_score, fill = treatment)) +
  ggplot2::geom_density(alpha = 0.5) +
  ggplot2::labs(
    title = "Distribution of propensity scores by treatment",
    x = "Propensity score",
    fill = "Treatment"
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave(
  filename = "PropensityScores/propensity_plot_rwd.png",
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

cond <- selectedLassoFeatures[grepl("cond", selectedLassoFeatures)]
cond <- gsub("[^0-9]", "", cond)
cdm_a$concept |> dplyr::filter(.data$concept_id %in% cond) |> dplyr::select(concept_id, concept_name) |> print(n = 100)






CDMConnector::cdmDisconnect(cdm_a)

CDMConnector::cdmDisconnect(cdm_g)
