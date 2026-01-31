# extract data
minFrequency <- 0.005
excludeCodes <- c(0, unlist(exclude, use.names = FALSE), codelist$radiotheraphy, codelist$prostatectomy)

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
  collect()
gleason <- cdm$gleason |>
  inner_join(
    cdm$my_cohort |>
      select(subject_id, index_date = cohort_start_date) |>
      distinct(),
    by = "subject_id"
  ) |>
  mutate(time = date_count_between(index_date, cohort_start_date)) |>
  select(subject_id, time, gleason_category) |>
  collect()
