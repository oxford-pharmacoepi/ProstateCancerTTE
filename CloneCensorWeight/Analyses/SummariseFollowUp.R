# follow up time
ot <- cdm$my_cohort |>
  addCohortName() |>
  group_by(cohort_name, follow_up, follow_up_reason) |>
  tally() |>
  collect()
total <- cdm$my_cohort |>
  addCohortName() |>
  group_by(cohort_name) |>
  tally() |>
  collect()
time <- 0:(365 * 5)
followUpTime <- tibble(time = time)
cols <- c("untreated", "surveillance_4_months", "surveillance_6_months", "prostatectomy", "radiotheraphy")
for (col in cols) {
  followUpTime <- followUpTime |>
    mutate(!!col := map_dbl(time, \(x) {
      ot |>
        filter(cohort_name == col, follow_up > x) |>
        summarise(n = sum(n)) |>
        pull() / total$n[total$cohort_name == col] * 100
    }))
}
followUpTime <- followUpTime |>
  pivot_longer(
    cols = cols,
    names_to = "cohort_name",
    values_to = "percentage"
  ) |>
  rename(variable_level = time) |>
  mutate(
    percentage = round(percentage, 2),
    variable_name = "Contributing individuals", 
    cdm_name = cdmName(cdm),
    variable_level = sprintf("%.0f", variable_level),
    result_type = "follow_up_time"
  ) |>
  transformToSummarisedResult(
    group = "cohort_name",
    estimates = "percentage",
    settings = "result_type"
  )

# follow up by reason
followUpTime2 <- cdm$my_cohort |>
  addCohortName() |>
  summariseResult(
    group = "cohort_name",
    strata = "follow_up_reason",
    variables = list("follow_up", "follow_up_reason"),
    estimates = list(c("min", "q05", "q25", "median", "q75", "q95", "max"), c("count", "percentage"))
  ) |>
  addResultType("follow_up_reason")

# follow up over time by reason
time <- 0:(365 * 5)
cols <- c("untreated", "surveillance_4_months", "surveillance_6_months", "prostatectomy", "radiotheraphy")
followUpTime3 <- list()
for (col in cols) {
  followUpTime3[[col]] <- map(time, \(x) {
    ot |>
      filter(cohort_name == col) |>
      mutate(status = if_else(
        follow_up > x,
        "in_observation",
        follow_up_reason
      )) |>
      group_by(cohort_name, status) |>
      summarise(n = sum(n), .groups = "drop") |>
      mutate(percentage = 100 * n / sum(n), time = x)
  }) |>
    bind_rows()
}
followUpTime3 <- followUpTime3 |>
  bind_rows() |>
  rename(variable_level = time, variable_name = status) |>
  mutate(
    percentage = round(percentage, 2),
    cdm_name = cdmName(cdm),
    variable_level = sprintf("%.0f", variable_level),
    result_type = "follow_up_reason_time"
  ) |>
  transformToSummarisedResult(
    group = "cohort_name",
    estimates = "percentage",
    settings = "result_type"
  )

# weighted follow up over time
followUp <- cdm$my_cohort |>
  addCohortName() |>
  select("cohort_name", "subject_id", "follow_up") |>
  collect() |>
  changeIds()
followUpTime4 <- weightTypes |>
  map(\(wt) {
    comparisons |>
      pmap(\(reference, exposed, comparison_id, comparison_name) {
        wgt <- getWeights(comparison_id, wt) |>
          inner_join(followUp, by = c("cohort_name", "subject_id")) |>
          mutate(time_end = if_else(time_end > follow_up, follow_up, time_end)) |>
          filter(time_start < time_end)
        time |>
          map(\(t) {
            wgt |>
              filter(time_start < t & t <= time_end) |>
              group_by(cohort_name) |>
              summarise(weighted_individuals = sum(weight), .groups = "drop") |>
              mutate(time = t)
          }) |>
          bind_rows() |>
          mutate(
            comparison_id = comparison_id,
            reference = reference,
            exposed = exposed
          )
      }) |>
      bind_rows() |>
      mutate(weight_type = wt)
  }) |>
  bind_rows() |>
  rename(variable_level = "time") |>
  mutate(
    variable_name = "Weighted individuals in observation",
    weighted_individuals = round(weighted_individuals, 1),
    cdm_name = cdmName(cdm),
    variable_level = sprintf("%.0f", variable_level),
    result_type = "follow_up_over_time"
  ) |>
  transformToSummarisedResult(
    group = c("comparison_id", "reference", "exposed"),
    strata = "cohort_name",
    additional = "weight_type",
    estimates = "weighted_individuals",
    settings = "result_type"
  )

results$follow_up <- bind(followUpTime, followUpTime2, followUpTime3, followUpTime4)
