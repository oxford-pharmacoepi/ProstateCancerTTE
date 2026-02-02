# extract data
minFrequency <- 0.005
excludeCodes <- c(0, unlist(exclude, use.names = FALSE), codelist$radiotheraphy, codelist$prostatectomy)

cdm$my_cohort  <- cdm$my_cohort  |>
  mutate(index_year = get_year(cohort_start_date)) |>
  compute(name = "my_cohort")
individuals <- cdm$my_cohort |>
  group_by(subject_id, cohort_start_date) |>
  summarise(max_censor = max(follow_up), .groups = "drop") |>
  rename(person_id = subject_id)
total_ind <- individuals |>
  tally() |>
  pull() |>
  as.numeric()

conditions <- extractCovariates("condition_occurrence", individuals, total_ind, minFrequency, excludeCodes) |>
  mutate(time = if_else(time < 0, 0, time)) |>
  distinct()
drugs <- extractCovariates("drug_exposure", individuals, total_ind, minFrequency, excludeCodes) |>
  filter(time >= -365)
psa <- cdm$psa |>
  inner_join(
    cdm$my_cohort |>
      select(subject_id, index_date = cohort_start_date) |>
      distinct(),
    by = "subject_id"
  ) |>
  mutate(time = date_count_between(index_date, cohort_start_date)) |>
  select(subject_id, time, psa_category) |>
  collect() |>
  mutate(psa_category = case_when(
    psa_category == "[0, 3)" ~ 0,
    psa_category == "[3, 6)" ~ 1,
    psa_category == "[6, 10)" ~ 2,
    psa_category == "[10, 20)" ~ 3,
    psa_category == "[20, 40)" ~ 4,
    psa_category == "[40, Inf)" ~ 5,
    .default = NA
  ))
gleason <- cdm$gleason |>
  inner_join(
    cdm$my_cohort |>
      select(subject_id, index_date = cohort_start_date) |>
      distinct(),
    by = "subject_id"
  ) |>
  mutate(time = date_count_between(index_date, cohort_start_date)) |>
  select(subject_id, time, gleason_category) |>
  collect() |>
  mutate(gleason_category = case_when(
    gleason_category == "<2" ~ 0,
    gleason_category == "2 to 6" ~ 1,
    gleason_category == "7" ~ 2,
    gleason_category == "8 to 10" ~ 3,
    gleason_category == ">10" ~ 4,
    .default = NA
  ))

# prepare subjects
cohort <- cdm$my_cohort |>
  addDateOfBirthQuery() |>
  mutate(
    year_of_birth = get_year(date_of_birth),
    index_year = get_year(cohort_start_date)
  ) |>
  addCohortName() |>
  select(cohort_name, subject_id, stage, follow_up, year_of_birth, index_year) |>
  collect()

coef <- list()
weights <- list()
weights[["0"]] <- cohort |>
  mutate(prob = 1, weight = 1, time = 0) |>
  select(subject_id, prob, weight, cohort_name, time)
for (ti in seq(from = 10, to = 1000, by = 10)) {
  tictoc::tic()
  x <- createCovariatesMatrix(cohort, ti, drugs, conditions, psa, gleason)
  xm <- modelWeights(x, ti)
  coef[[as.character(ti)]] <- xm$coef
  weights[[as.character(ti)]] <- xm$weights
  tictoc::toc()
}
coef <- bind_rows(coef)
weights <- bind_rows(weights)

# export coefficients
concepts <- coef |>
  filter(startsWith(variable, "cov_")) |>
  mutate(concept_id = as.numeric(gsub("cov_", "", variable))) |>
  distinct(concept_id) |>
  pull()
concepts <- cdm$concept |>
  filter(concept_id %in% concepts) |>
  select(variable = concept_id, concept_name) |>
  collect() |>
  mutate(
    concept_name = paste0(concept_name, " (", variable, ")"),
    variable = paste0("cov_", variable)
  )
coef <- coef |>
  left_join(concepts, by = "variable") |>
  mutate(
    cdm_name = cdmName(cdm),
    variable_name = coalesce(concept_name, variable),
    variable_level = NA_character_,
    result_type = "coefficients"
  ) |>
  transformToSummarisedResult(
    group = "cohort_name",
    strata = "time",
    estimates = "coef",
    settings = "result_type"
  )
