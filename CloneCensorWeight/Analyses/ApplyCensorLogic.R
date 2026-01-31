
cdm$prostate_cancer <- cdm$prostate_cancer |>
  addFutureObservation(name = "prostate_cancer") |>
  addCohortIntersectDays(
    targetCohortTable = "treatments",
    name = "prostate_cancer",
    order = "first",
    nameStyle = "{cohort_name}"
  ) |>
  addCohortIntersectDays(
    targetCohortTable = "censor",
    name = "prostate_cancer",
    order = "first",
    nameStyle = "{cohort_name}"
  ) |>
  addCohortIntersectDays(
    targetCohortTable = "death_cohort",
    name = "prostate_cancer",
    order = "first",
    nameStyle = "{cohort_name}"
  ) |>
  mutate(across(
    c("prostatectomy", "radiotheraphy", "censor_event", "death_cohort"),
    \(x) coalesce(x, 9999)
  )) |>
  compute(name = "prostate_cancer") |>
  mutate(censor_event = if_else(censor_event <= 365, censor_event, 9999)) |>
  compute(name = "prostate_cancer")

# surveillance arm
cdm$surveillance <- cdm$prostate_cancer |>
  compute(name = "surveillance") |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1L,
      cohort_name = "surveillance"
    )
  ) |>
  mutate(
    follow_up = case_when(
      prostatectomy <= future_observation & prostatectomy <= radiotheraphy & prostatectomy <= censor_event & prostatectomy <= death_cohort ~ prostatectomy,
      radiotheraphy <= future_observation & radiotheraphy <= censor_event & radiotheraphy <= death_cohort ~ radiotheraphy,
      death_cohort <= future_observation & death_cohort <= censor_event ~ death_cohort,
      censor_event <= future_observation ~ censor_event,
      .default = future_observation
    ),
    follow_up_reason = case_when(
      follow_up == prostatectomy ~ "prostatectomy",
      follow_up == radiotheraphy ~ "radiotheraphy",
      follow_up == death_cohort ~ "death",
      follow_up == censor_event ~ "censor",
      .default = "future_observation"
    )
  ) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, stage, follow_up, follow_up_reason) |>
  compute(name = "surveillance")

# prostatectomy arm
cdm$prostatectomy <- cdm$prostate_cancer |>
  compute(name = "prostatectomy") |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1L,
      cohort_name = "prostatectomy"
    )
  ) |>
  mutate(
    prostatectomy_censor = if_else(prostatectomy <= 365, 9999, 365),
    follow_up = case_when(
      prostatectomy_censor <= future_observation & prostatectomy_censor <= radiotheraphy & prostatectomy_censor <= censor_event & prostatectomy_censor <= death_cohort ~ prostatectomy_censor,
      radiotheraphy <= future_observation & radiotheraphy <= censor_event & radiotheraphy <= death_cohort ~ radiotheraphy,
      death_cohort <= future_observation & death_cohort <= censor_event ~ death_cohort,
      censor_event <= future_observation ~ censor_event,
      .default = future_observation
    ),
    follow_up_reason = case_when(
      follow_up == prostatectomy_censor ~ "no prostatectomy",
      follow_up == radiotheraphy ~ "radiotheraphy",
      follow_up == death_cohort ~ "death",
      follow_up == censor_event ~ "censor",
      .default = "future_observation"
    )
  ) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, stage, follow_up, follow_up_reason) |>
  compute(name = "prostatectomy")

# radiotheraphy arm
cdm$radiotheraphy <- cdm$prostate_cancer |>
  compute(name = "radiotheraphy") |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1L,
      cohort_name = "radiotheraphy"
    )
  ) |>
  mutate(
    radiotheraphy_censor = if_else(radiotheraphy <= 365, 9999, 365),
    follow_up = case_when(
      radiotheraphy_censor <= future_observation & radiotheraphy_censor <= prostatectomy & radiotheraphy_censor <= censor_event & radiotheraphy_censor <= death_cohort ~ radiotheraphy_censor,
      prostatectomy <= future_observation & prostatectomy <= censor_event & prostatectomy <= death_cohort ~ prostatectomy,
      death_cohort <= future_observation & death_cohort <= censor_event ~ death_cohort,
      censor_event <= future_observation ~ censor_event,
      .default = future_observation
    ),
    follow_up_reason = case_when(
      follow_up == radiotheraphy_censor ~ "no radiotheraphy",
      follow_up == prostatectomy ~ "prostatectomy",
      follow_up == death_cohort ~ "death",
      follow_up == censor_event ~ "censor",
      .default = "future_observation"
    )
  ) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, stage, follow_up, follow_up_reason) |>
  compute(name = "radiotheraphy")

# no outcome death
cdm <- bind(cdm$surveillance, cdm$prostatectomy, cdm$radiotheraphy, name = "my_cohort")

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
followUpTime <- tibble(time = time) |>
  mutate(
    surveillance = map_dbl(time, \(x) {
      ot |>
        filter(cohort_name == "surveillance", follow_up > x) |>
        summarise(n = sum(n)) |>
        pull() / total$n[total$cohort_name == "surveillance"] * 100
    }),
    prostatectomy = map_dbl(time, \(x) {
      ot |>
        filter(cohort_name == "prostatectomy", follow_up > x) |>
        summarise(n = sum(n)) |>
        pull() / total$n[total$cohort_name == "prostatectomy"] * 100
    }),
    radiotheraphy = map_dbl(time, \(x) {
      ot |>
        filter(cohort_name == "radiotheraphy", follow_up > x) |>
        summarise(n = sum(n)) |>
        pull() / total$n[total$cohort_name == "radiotheraphy"] * 100
    })
  ) |>
  pivot_longer(
    c("surveillance", "prostatectomy", "radiotheraphy"),
    names_to = "cohort_name",
    values_to = "percentage"
  ) |>
  rename(variable_level = time) |>
  mutate(
    percentage = sprintf("%.2f", percentage),
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
