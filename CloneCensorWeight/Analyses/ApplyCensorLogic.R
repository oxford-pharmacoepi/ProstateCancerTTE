
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
      prostatectomy <= future_observation & prostatectomy <= radiotheraphy & prostatectomy <= censor_event & prostatectomy <= death_cohort ~ prostatectomy,
      prostatectomy <= future_observation & prostatectomy <= radiotheraphy & prostatectomy <= censor_event & prostatectomy <= death_cohort ~ prostatectomy,
    )
  )
