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
