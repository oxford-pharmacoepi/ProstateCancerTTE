# prepare outcomes
outcomes <- cdm$outcomes |>
  addCohortName() |>
  select(subject_id, outcome_date = cohort_start_date, outcome = cohort_name) |>
  inner_join(
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
  ungroup() |>
  changeIds()

# prepare nco
nco <- cdm$nco |>
  addCohortName() |>
  select(subject_id, outcome_date = cohort_start_date, outcome = cohort_name) |>
  inner_join(
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
  ungroup() |>
  changeIds()

# prepare death
do <- cdm$death_cohorts |>
  addCohortName() |>
  select(subject_id, outcome_date = cohort_start_date, outcome = cohort_name) |>
  inner_join(
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
  ungroup() |>
  changeIds()

# join outcomes
outcomes <- outcomes |>
  union_all(do) |>
  mutate(outcome_type = "main") |>
  union_all(
    nco |>
      mutate(outcome_type = "nco")
  )

# prepare timings
weights <- weights |>
  select(!any_of(c("psa_at_index", "psa_at_weight", "gleason_at_index", "gleason_at_weight", "age"))) |>
  mutate(time_end = time + 10) |>
  mutate(time_end = if_else(follow_up <= time_end, follow_up, time_end)) |>
  select(!"follow_up")

# calculate outcome model
outcomeModel <- summariseOutcomeModel(weights, outcomes, cdmName(cdm))
