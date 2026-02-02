# prepare outcomes
outcomes <- cdm$outcomes |>
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

cohorts <- unique(cohort$cohort_name)
comparisons <- expand_grid(reference = seq_along(cohorts), comparator = seq_along(cohorts)) |>
  filter(reference < comparator) |>
  mutate(reference = cohorts[reference], comparator = cohorts[comparator])

# prepare timings
weights <- weights |>
  select(!"prob") |>
  mutate(time_end = time + 10) |>
  left_join(
    cohort |>
      select(cohort_name, subject_id, follow_up),
    by = c("cohort_name", "subject_id")
  ) |>
  filter(time < follow_up) |>
  mutate(time_end = if_else(follow_up <= time_end, follow_up, time_end)) |>
  select(!"follow_up")

result <- unique(outcomes$outcome) |>
  map(\(out) {
    cli_inform(c(i = "Fitting model for outcome: {.pkg {out}}"))
    xo <- outcomes |>
      filter(outcome == out) |>
      select(subject_id, out_time = time)
    data <- weights |>
      left_join(xo, by = "subject_id") |>
      mutate(
        out_time = coalesce(out_time, 9999),
        time_end = if_else(out_time <= time_end, out_time, time_end),
        status = if_else(time_end == out_time, 1, 0)
      ) |>
      filter(time < time_end) |>
      select(!"out_time")
    pmap(comparisons, \(reference, comparator) {
      xd <- data |>
        filter(cohort_name %in% c(comparator, reference)) |>
        mutate(cohort_name = factor(cohort_name, c(reference, comparator)))
      fit <- coxph(Surv(time, time_end, status) ~ cohort_name,
                   data = xd,
                   weights = weight,
                   cluster = subject_id)
      summary(fit) |>
        coefficients() |>
        as_tibble(rownames = "comparator") |>
        mutate(reference = reference) |>
        rename("se_coef" = "se(coef)") |>
        mutate(comparator = str_replace(comparator, "cohort_name", "")) |>
        select(reference, comparator, coef, se_coef)
    }) |>
      bind_rows() |>
      mutate(outcome = out)
  })

outcomeModel <- result |>
  bind_rows() |>
  mutate(
    cdm_name = cdmName(cdm),
    variable_name = "Cox model",
    variable_level = NA_character_,
    result_type = "cox_regression",
    hr = exp(coef),
    hr_lower = exp(coef - 1.96 * se_coef),
    hr_upper = exp(coef + 1.96 * se_coef)
  ) |>
  transformToSummarisedResult(
    group = c("reference", "comparator"),
    strata = "outcome",
    estimates = c("hr", "hr_lower", "hr_upper", "coef", "se_coef"),
    settings = "result_type"
  )
