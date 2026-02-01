# prepare outcomes
outcomes <- cdm$outcome |>
  addCohortName() |>
  select(subject_id, outcome_date = cohort_start_date, outcome = cohort_name) |>
  left_join(
    cdm$my_cohort |>
      distinct(subject_id, index_date = cohort_start_date),
    by = "subject_id"
  ) |>
  mutate(time = date_count_between(index_date, outcome_date)) |>
  filter(time >= 0) |>
  select(subject_id, time, outcome) |>
  collect() |>
  group_by(subject_id, outcome) |>
  filter(time == min(time, na.rm = TRUE)) |>
  ungroup()
