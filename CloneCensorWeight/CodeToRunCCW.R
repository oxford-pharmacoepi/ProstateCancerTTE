library(RPostgres)
library(CDMConnector)
library(omopgenerics, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(here)
library(CohortConstructor)
library(PatientProfiles)
library(glmnet)
library(purrr)
library(nnet)
library(cli)
library(survival)
library(stringr)

#cdmName <- "CPRD Aurum"
cdmName <- "CPRD GOLD"

con <- dbConnect(drv = Postgres(),
                 dbname = if_else(cdmName == "CPRD GOLD", "cdm_gold_p22_001867", "cdm_aurum_p22_001867"),
                 host = Sys.getenv("DB_HOST"),
                 port = Sys.getenv("DB_PORT"),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

cdm <- cdmFromCon(
  con = con,
  cdmName = cdmName,
  cdmSchema = "public",
  writeSchema = "results",
  writePrefix = "mc_pca_",
  .softValidation = TRUE
)

# filter observation_period
cdm$observation_period <- cdm$observation_period |>
  filter(period_type_concept_id == 32882)
cdm <- validateCdmArgument(
  cdm = cdm,
  checkOverlapObservation = TRUE,
  checkStartBeforeEndObservation = TRUE,
  checkPlausibleObservationDates = TRUE,
  checkPerson = TRUE
)

# read codelists
codelist <- importCodelist(here("Codelist", "InclusionCriteria"), type = "csv")
names(codelist)[names(codelist) == "ebrt"] <- "radiotheraphy"
names(codelist)[names(codelist) == "radical_prostatectomy"] <- "prostatectomy"
names(codelist)[names(codelist) == "stage3_4"] <- "progression"

# create cohorts
nm <- "prostate_cancer"
cdm[[nm]] <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist["prostate_cancer"],
  name = nm
) |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireFutureObservation(minFutureObservation = 1) |>
  addDemographics(
    sex = FALSE,
    priorObservation = FALSE,
    name = nm
  ) |>
  addDeathDays(name = nm) |>
  mutate(future_observation = if_else(
    is.na(days_to_death) | days_to_death >= future_observation,
    future_observation,
    days_to_death
  )) |>
  addConceptIntersectDays(
    conceptSet = codelist[c("prostatectomy", "radiotheraphy", "progression")],
    window = c(-Inf, Inf),
    nameStyle = "{concept_name}",
    order = "first",
    name = nm
  ) |>
  filter(is.na(prostatectomy) | prostatectomy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `prostatectomy` before index date") |>
  filter(is.na(radiotheraphy) | radiotheraphy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `radiotheraphy` before index date") |>
  filter(is.na(progression) | progression > 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `progression` before or on index date") |>
  copyCohorts(name = nm, n = 3) |>
  renameCohort(cohortId = "prostate_cancer", newCohortName = "surveillance") |>
  renameCohort(cohortId = "prostate_cancer_1", newCohortName = "prostatectomy") |>
  renameCohort(cohortId = "prostate_cancer_2", newCohortName = "radiotheraphy")
surveillanceId <- getCohortId(cohort = cdm[[nm]], cohortName = "surveillance")
prostatectomyId <- getCohortId(cohort = cdm[[nm]], cohortName = "prostatectomy")
radiotheraphyId <- getCohortId(cohort = cdm[[nm]], cohortName = "radiotheraphy")
cdm[[nm]] <- cdm[[nm]] |>
  addCohortName() |>
  filter(is.na(prostatectomy) | prostatectomy > 0 | cohort_name == "prostatectomy") |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `prostatectomy` on index date", cohortId = c(surveillanceId, radiotheraphyId)) |>
  filter(is.na(radiotheraphy) | radiotheraphy > 0 | cohort_name == "radiotheraphy") |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `radiotheraphy` on index date", cohortId = c(surveillanceId, prostatectomyId))

# basic censoring
cdm[[nm]] <- cdm[[nm]] |>
  mutate(
    prostatectomy = coalesce(prostatectomy, 9999),
    radiotheraphy = coalesce(radiotheraphy, 9999),
    progression = coalesce(progression, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    censor_time = case_when(
      days_to_death <= future_observation & days_to_death <= progression ~ days_to_death,
      progression <= future_observation ~ progression,
      .default = future_observation
    ),
    censor_time = case_when(
      cohort_name == "surveillance" & prostatectomy <= censor_time & prostatectomy <= radiotheraphy ~ prostatectomy,
      cohort_name == "surveillance" & radiotheraphy <= censor_time & radiotheraphy <= prostatectomy ~ radiotheraphy,
      cohort_name == "surveillance" ~ censor_time,
      cohort_name == "prostatectomy" & prostatectomy <= 365 ~ if_else(radiotheraphy <= censor_time, radiotheraphy, censor_time),
      cohort_name == "prostatectomy" ~ if_else(365 <= censor_time, 365, censor_time),
      cohort_name == "radiotheraphy" & radiotheraphy <= 365 ~ if_else(prostatectomy <= censor_time, prostatectomy, censor_time),
      cohort_name == "radiotheraphy" ~ if_else(365 <= censor_time, 365, censor_time)
    ),
    censor_reason = case_when(
      censor_time == prostatectomy & cohort_name != "prostatectomy" ~ "prostatectomy",
      censor_time == radiotheraphy & cohort_name != "radiotheraphy" ~ "radiotheraphy",
      censor_time == progression ~ "progression",
      censor_time == days_to_death ~ "death",
      censor_time == future_observation ~ "end observation",
      censor_time == 365 ~ "no treatment",
      .default = "error"
    )
  ) |>
  compute(name = nm)

min_frequency <- 0.005
num_subjects <- cdm[[nm]] |>
  distinct(subject_id) |>
  tally() |>
  pull() |>
  as.numeric()
min_subjects <- num_subjects * min_frequency

# eclude concepts from propensity scores
conceptsToExclude <- c(
  codelist$radiotheraphy, codelist$prostate_cancer, codelist$prostatectomy
)

# extract conditions
conditions <- cdm$condition_occurrence |>
  select(subject_id = "person_id", "condition_start_date", concept_id = "condition_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      group_by(subject_id) |>
      summarise(
        cohort_start_date = min(cohort_start_date, na.rm = TRUE),
        censor_time = max(censor_time, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "subject_id"
  ) |>
  mutate(cov_time = condition_start_date - cohort_start_date) |>
  filter(cov_time < censor_time) |>
  distinct(subject_id, cov_time, concept_id) |>
  collect()
conds <- conditions |>
  group_by(concept_id) |>
  summarise(n_subjects = n_distinct(subject_id)) |>
  filter(concept_id != 0) |>
  filter(n_subjects >= min_subjects) |>
  filter(!concept_id %in% conceptsToExclude) |>
  pull(concept_id)
conditions <- conditions |>
  filter(concept_id %in% conds)

# extract exposures
exposures <- cdm$drug_exposure |>
  select(subject_id = "person_id", "drug_exposure_start_date", concept_id = "drug_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      group_by(subject_id) |>
      summarise(
        cohort_start_date = min(cohort_start_date, na.rm = TRUE),
        censor_time = max(censor_time, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "subject_id"
  ) |>
  mutate(cov_time = drug_exposure_start_date - cohort_start_date) |>
  filter(cov_time < censor_time & cov_time >= -365) |>
  distinct(subject_id, cov_time, concept_id) |>
  collect()
exps <- exposures |>
  group_by(concept_id) |>
  summarise(n_subjects = n_distinct(subject_id)) |>
  filter(concept_id != 0) |>
  filter(n_subjects >= min_subjects) |>
  filter(!concept_id %in% conceptsToExclude) |>
  pull(concept_id)
exposures <- exposures |>
  filter(concept_id %in% exps)

# collect data
x <- cdm[[nm]] |>
  select("cohort_name", "subject_id", "age", death = "days_to_death", "censor_time", "censor_reason") |>
  collect() |>
  mutate(cohort_name = factor(cohort_name, levels = c("surveillance", "prostatectomy", "radiotheraphy")))

outcome <- "death"
x <- x |>
  rename(outcome = all_of(outcome)) |>
  mutate(
    status = if_else(is.na(outcome) | outcome > censor_time, 0, 1),
    time = if_else(status == 0, censor_time, outcome),
    reason = if_else(status == 0, censor_reason, "outcome")
  ) |>
  select("cohort_name", "subject_id", "status", "time", "reason", "age")

sep <- 30
weights_time <- seq(from = 0, to = 730, by = sep)

coefficients <- list()
probabilities <- list()

for (wti in weights_time) {
  start_time <- as.numeric(Sys.time())

  xi <- x |>
    filter(time > wti)

  min_counts_i <- nrow(xi) * min_frequency

  # exposures
  exposures_i <- exposures |>
    filter(cov_time <= wti & cov_time + 365 >= wti) |>
    inner_join(
      xi |>
        select("cohort_name", "subject_id"),
      by = "subject_id",
      relationship = "many-to-many"
    ) |>
    distinct(cohort_name, subject_id, concept_id)
  exps_i <- exposures_i |>
    group_by(concept_id) |>
    tally() |>
    filter(n >= min_counts_i) |>
    pull(concept_id)
  exposures_i <- exposures_i |>
    filter(concept_id %in% exps_i) |>
    mutate(value = 1, concept_id = paste0("cov_", concept_id)) |>
    pivot_wider(names_from = "concept_id", values_from = "value")

  # conditions
  conditions_i <- conditions |>
    filter(cov_time <= wti) |>
    inner_join(
      xi |>
        select("cohort_name", "subject_id"),
      by = "subject_id",
      relationship = "many-to-many"
    ) |>
    distinct(cohort_name, subject_id, concept_id)
  cond_i <- conditions_i |>
    group_by(concept_id) |>
    tally() |>
    filter(n >= min_counts_i) |>
    pull(concept_id)
  conditions_i <- conditions_i |>
    filter(concept_id %in% cond_i) |>
    mutate(value = 1, concept_id = paste0("cov_", concept_id)) |>
    pivot_wider(names_from = "concept_id", values_from = "value")

  # create matrix
  covs <- paste0("cov_", c(cond_i, exps_i))
  xi <- xi |>
    left_join(conditions_i, by = c("cohort_name", "subject_id")) |>
    left_join(exposures_i, by = c("cohort_name", "subject_id")) |>
    mutate(across(all_of(covs), \(x) coalesce(x, 0)))

  # lasso to select variables
  X <- xi |>
    select(!c("cohort_name", "subject_id", "status", "time", "reason", "age")) |>
    as.matrix()
  y <- xi$cohort_name
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg <- cv.glmnet(x = X, y = y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "multinomial", alpha = 1)
  selected_cov <- coef(lasso_reg, s = lasso_reg$lambda.1se) |>
    map(\(x) {
      x <- names(x[(x[,1]!=0),1])
      x[x != "(Intercept)"]
    }) |>
    unlist(use.names = FALSE) |>
    unique()

  # logistic regression
  X <- xi |>
    select(all_of(c("cohort_name", "age", selected_cov)))
  fit <- multinom(cohort_name ~ ., data = X, trace = FALSE)

  # save model
  coefficients[[as.character(wti)]] <- fit |>
    coefficients() |>
    as_tibble(rownames = "group") |>
    pivot_longer(!"group", names_to = "covariate", values_to = "value")

  # save weights
  probs <- predict(fit, type = "probs")
  probabilities[[as.character(wti)]] <- xi |>
    select("cohort_name", "subject_id") |>
    bind_cols(as_tibble(probs))

  elapsed_time <- round(as.numeric(Sys.time()) - start_time)
  cli_inform(c("i" = "Finished weighting for time = {wti} in {elapsed_time} seconds."))
}

# survival statistics
# number events
# number weighted events
# number subjects over time
# number weighted subjects over time
# censoring summary

# survival over time
surv_data <- probabilities |>
  bind_rows(.id = "time_start") |>
  mutate(
    weight = case_when(
      cohort_name == "surveillance" ~ 1 - surveillance,
      cohort_name == "prostatectomy" ~ 1 - prostatectomy,
      cohort_name == "radiotheraphy" ~ 1 - radiotheraphy
    ),
    time_start = as.numeric(time_start),
    time_end = time_start + sep
  ) |>
  inner_join(
    x |>
      select(!"age"),
    by = c("cohort_name", "subject_id")
  ) |>
  mutate(
    status = if_else(time_start < time & time <= time_end, status, 0),
    time_end = if_else(time_start < time & time <= time_end, time, time_end)
  ) |>
  select(!c("surveillance", "prostatectomy", "radiotheraphy"))
fit <- survfit(Surv(time_start, time_end, status) ~ cohort_name, data = surv_data, weights = weight)
survival_summary <- summary(fit, times = c(0:max(surv_data$time_end)))
survival_summary <- tibble(
  time = survival_summary$time,
  survival = survival_summary$surv,
  lower_ci = survival_summary$lower,
  upper_ci = survival_summary$upper,
  cohort = str_replace(survival_summary$strata, "^cohort_name=", "")
) |>
  mutate(outcome = outcome)

# export coefficients
coefficients <- coefficients |>
  bind_rows(.id = "time_start") |>
  mutate(time_start = as.numeric(time_start), outcome = outcome)

# export probabilities
hist_sep <- 0.01
breaks <- seq(from = 0, to = 1, by = hist_sep)
labels <- as.character(breaks[-1] - hist_sep/2)
probabilities <- probabilities |>
  map(\(x) {
    x |>
      pivot_longer(
        cols = c("surveillance", "prostatectomy", "radiotheraphy"),
        names_to = "prob_label",
        values_to = "prob"
      ) |>
      select(!"subject_id")
  }) |>
  bind_rows(.id = "time_start") |>
  mutate(prob_bin = as.character(cut(x = prob, breaks = breaks, labels = labels))) |>
  group_by(time_start, cohort_name, prob_label, prob_bin) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(time_start, cohort_name, prob_label) |>
  mutate(freq = n / sum(n)) |>
  ungroup() |>
  select(!"n") |>
  full_join(
    expand_grid(
      time_start = names(probabilities),
      cohort_name = c("surveillance", "prostatectomy", "radiotheraphy"),
      prob_label = c("surveillance", "prostatectomy", "radiotheraphy"),
      prob_bin = labels
    ),
    by = c("time_start", "cohort_name", "prob_label", "prob_bin")
  ) |>
  mutate(
    freq = coalesce(freq, 0),
    time_start = as.numeric(time_start),
    prob_bin = as.numeric(prob_bin),
    outcome = outcome
  ) |>
  arrange(time_start, cohort_name, prob_label, prob_bin)

save(survival_summary, coefficients, probabilities, file = "result.RData")
