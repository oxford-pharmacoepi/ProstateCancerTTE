# extract data
minFrequency <- 0.005
excludeCodes <- c(0, unlist(exclude, use.names = FALSE), codelist$radiotheraphy, codelist$prostatectomy)

cdm$my_cohort  <- cdm$my_cohort  |>
  mutate(index_year = get_year(cohort_start_date)) |>
  compute(name = "my_cohort")
individuals <- cdm$my_cohort |>
  group_by(subject_id, cohort_start_date) |>
  summarise(max_censor = max(follow_up, na.rm = TRUE), .groups = "drop") |>
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
  addAgeQuery() |>
  mutate(index_year = get_year(cohort_start_date)) |>
  addCohortName() |>
  select(cohort_name, subject_id, follow_up, age, index_year) |>
  collect()

# recalculate subject_id
ids <- cohort |>
  distinct(subject_id) |>
  arrange(subject_id) |>
  mutate(new_id = row_number())
changeIds <- function(x) {
  x |>
    inner_join(ids, by = "subject_id") |>
    select(!"subject_id") |>
    rename(subject_id = "new_id")
}
cohort <- changeIds(cohort)
conditions <- changeIds(conditions)
drugs <- changeIds(drugs)
psa <- changeIds(psa)
gleason <- changeIds(gleason)

# IPCW
coefIPCW <- list()
weightsIPCW <- list()
weightsIPCW[["0"]] <- cohort |>
  mutate(prob = 1, weight = 1, time = 0) |>
  select(subject_id, prob, weight, cohort_name, time)
for (ti in seq(from = 10, to = 1000, by = 10)) {
  startTime <- Sys.time()
  cat(paste0("IPCW at time \033[34m\033[1m", ti, "\033[0m"))
  x <- createCovariatesMatrix(cohort, ti, drugs, conditions, psa, gleason)
  xm <- modelWeights(x, ti)
  coefIPCW[[as.character(ti)]] <- xm$coef
  weightsIPCW[[as.character(ti)]] <- xm$weights
  endTime <- Sys.time()
  td <- sprintf("%.1f", difftime(time = endTime, time2 = startTime, units = "secs"))
  cat(paste0(" finished in \033[3m", td, " seconds.\033[0m\n"))
}
coefIPCW <- bind_rows(coefIPCW)
weightsIPCW <- bind_rows(weightsIPCW)

# IPTW
coefIPTW <- list()
weightsIPTW <- list()
w0 <- cohort |>
  mutate(prob = 1, weight = 1, time = 0) |>
  select(subject_id, prob, weight, cohort_name, time)
cohorts <- unique(cohort$cohort_name)
comparisons <- expand_grid(
  reference = cohorts,
  comparator = cohorts
) |>
  filter(reference != comparator)
weightsIPTW[["0"]] <- w0 |>
  cross_join(comparisons) |>
  filter(cohort_name == reference | cohort_name == comparator)

for (ti in seq(from = 10, to = 1000, by = 10)) {
  tictoc::tic()
  cli_inform(c(i = "IPTW at time {.pkg {ti}}"))
  x <- createCovariatesMatrix(cohort, ti, drugs, conditions, psa, gleason)
  
  for (k in seq_len(nrow(comparisons))) {
    ref <- comparisons$reference[k]
    comp <- comparisons$comparator[k]
    xk <- x |>
      filter(cohort_name %in% c(ref, comp))
    if (length(unique(xk$cohort_name)) == 2) {
      xm <- xk |>
        mutate(
          status = if_else(cohort_name == ref, 0, 1),
          subject_id = paste0(cohort_name, "-", subject_id)
        ) |>
        select(!c("cohort_name", "follow_up")) |>
        calculateWeights() |>
        map(\(x) mutate(x, time = ti, reference = ref, comparator = comp))
      coefIPTW[[paste0(ti, ref, comp)]] <- xm$coef
      weightsIPTW[[paste0(ti, ref, comp)]] <- xm$weights |>
        mutate(
          cohort_name = str_extract(subject_id, "^[^-]+"),
          subject_id = as.integer(str_extract(subject_id, "(?<=-).*")),
          weight = if_else(reference == cohort_name, 1/(1 - prob), 1/prob)
        )
    }
  }
  tictoc::toc()
}
coefIPTW <- bind_rows(coefIPTW)
weightsIPTW <- bind_rows(weightsIPTW)

# merge coefficients and prepare to export
coef <- union_all(
  coefIPTW |>
    mutate(
      cohort_name = paste0(reference, " vs ", comparator),
      weight_type = "IPTW"
    ) |>
    select(!c("reference", "comparator")),
  coefIPCW |>
    mutate(weight_type = "IPCW")
)
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
    additional = "weight_type",
    estimates = "coef",
    settings = "result_type"
  )

# merge weights
weights <- weightsIPTW |>
  mutate(weight_type = "IPTW") |>
  select("weight_type", "reference", "comparator", "cohort_name", "subject_id", "time", "weight") |>
  union_all(
    weightsIPCW |>
      cross_join(comparisons) |>
      filter(cohort_name == reference | cohort_name == comparator) |>
      mutate(weight_type = "IPCW") |>
      select("weight_type", "reference", "comparator", "cohort_name", "subject_id", "time", "weight")
  ) |>
  union_all(
    weightsIPTW |>
      mutate(wt = weight) |>
      select(!c("prob", "weight")) |>
      inner_join(
        weightsIPCW |>
          mutate(wc = weight) |>
          select(!c("prob", "weight")),
        by = c("subject_id", "cohort_name", "time")
      ) |>
      mutate(
        weight_type = "IPTCW", 
        weight = wc * wt
      ) |>
      select("weight_type", "reference", "comparator", "cohort_name", "subject_id", "time", "weight")
  )

rm(weightsIPTW)
rm(weightsIPCW)

# prepare weights
weights <- weights |>
  left_join(
    cohort |>
      select("cohort_name", "subject_id", "follow_up", "age"),
    by = c("cohort_name", "subject_id")
  ) |>
  filter(time < follow_up)

save(weights, file = here("Results", "weights.RData"))
