# add variables of interest
weights <- weights |>
  # psa at index
  left_join(
    psa |>
      filter(time <= 0) |>
      group_by(subject_id) |>
      filter(time == max(time)) |>
      ungroup() |>
      select("subject_id", "psa_at_index" = "psa_category"),
    by = "subject_id"
  ) |>
  # psa at weights
  left_join(
    psa |>
      rename(psa_time = "time") |>
      inner_join(
        weights |>
          select("subject_id", "time") |>
          distinct(),
        by = "subject_id",
        relationship = "many-to-many"
      ) |>
      filter(psa_time <= time) |>
      group_by(subject_id, time) |>
      filter(psa_time == max(psa_time)) |>
      ungroup() |>
      select("subject_id", "time", "psa_at_weight" = "psa_category"),
    by = c("subject_id", "time")
  ) |>
  # gleason at index
  left_join(
    gleason |>
      filter(time <= 0) |>
      group_by(subject_id) |>
      filter(time == max(time)) |>
      ungroup() |>
      select("subject_id", "gleason_at_index" = "gleason_category"),
    by = "subject_id"
  ) |>
  # gleason at weights
  left_join(
    gleason |>
      rename(gleason_time = "time") |>
      inner_join(
        weights |>
          select("subject_id", "time") |>
          distinct(),
        by = "subject_id",
        relationship = "many-to-many"
      ) |>
      filter(gleason_time <= time) |>
      group_by(subject_id, time) |>
      filter(gleason_time == max(gleason_time)) |>
      ungroup() |>
      select("subject_id", "time", "gleason_at_weight" = "gleason_category"),
    by = c("subject_id", "time")
  )

cov0 <- conditions |>
  filter(time <= 0) |>
  distinct(subject_id, covariate) |>
  union_all(
    drugs |>
      filter(time <= 0 & time >= -365) |>
      distinct(subject_id, covariate)
  )

confoundingTime <- list()
time <- unique(weights$time)
unbalanced0 <- tibble(
  weight_type = character(),
  reference = character(),
  comparator = character(),
  covariate = character()
)
unbalancedT <- tibble(
  weight_type = character(),
  reference = character(),
  comparator = character(),
  covariate = character()
)
for (t in time) {
  
  cli_inform(c(i = "Confounding at time = {.pkg {t}}"))
  
  # individuals
  wt <- weights |>
    filter(time == t)
  tt <- sprintf("%s", t)
  
  # smds
  covT <- conditions |>
    filter(time <= t) |>
    distinct(subject_id, covariate) |>
    union_all(
      drugs |>
        filter(time <= t & time >= t-365) |>
        distinct(subject_id, covariate)
    )
  den <- wt |>
    group_by(weight_type, reference, comparator, cohort_name) |>
    summarise(total = sum(weight), .groups = "drop")
  
  # calculate smds at index
  smd0 <- wt |>
    select("weight_type", "reference", "comparator", "subject_id", "cohort_name", "weight") |>
    inner_join(cov0, by = "subject_id", relationship = "many-to-many") |>
    group_by(weight_type, reference, comparator, cohort_name, covariate) |>
    summarise(sum = sum(weight), .groups = "drop") |>
    left_join(den, by = c("weight_type", "reference", "comparator", "cohort_name")) |>
    mutate(p = sum / total) |>
    select("weight_type", "reference", "comparator", "cohort_name", "covariate", "p") |>
    calculateSmd()
  
  # unbalanced at index
  unbalanced0 <- unbalanced0 |>
    union_all(
      smd0 |>
        filter(unbalanced == 1) |>
        select("weight_type", "reference", "comparator", "covariate")
    )
  
  # calculate smds at weights
  smdT <- wt |>
    select("weight_type", "reference", "comparator", "subject_id", "cohort_name", "weight") |>
    inner_join(covT, by = "subject_id", relationship = "many-to-many") |>
    group_by(weight_type, reference, comparator, cohort_name, covariate) |>
    summarise(sum = sum(weight), .groups = "drop") |>
    left_join(den, by = c("weight_type", "reference", "comparator", "cohort_name")) |>
    mutate(p = sum / total) |>
    select("weight_type", "reference", "comparator", "cohort_name", "covariate", "p") |>
    calculateSmd()
  
  # unbalanced at weights
  unbalancedT <- unbalancedT |>
    union_all(
      smdT |>
        filter(unbalanced == 1) |>
        select("weight_type", "reference", "comparator", "covariate")
    )
  
  # age psa and gleason
  confoundingTime[[tt]] <- summariseResult(
    table = wt,
    group = list(c("weight_type", "reference", "comparator", "cohort_name")),
    variables = c("age", "psa_at_index", "psa_at_weight", "gleason_at_index", "gleason_at_weight"),
    estimates = c("min", "q25", "median", "q75", "max", "mean", "sd", "percentage_missing"),
    weights = "weight"
  ) |>
    suppressMessages() |>
    bind(
      summariseResult(
        counts = FALSE,
        table = wt |>
          mutate(across(
            c("psa_at_index", "psa_at_weight", "gleason_at_index", "gleason_at_weight"),
            as.character
          )),
        group = list(c("weight_type", "reference", "comparator", "cohort_name")),
        variables = c("psa_at_index", "psa_at_weight", "gleason_at_index", "gleason_at_weight"),
        estimates = c("percentage"),
        weights = "weight"
      ) |>
        suppressMessages()
    ) |>
    bind(
      # smd at index
      smd0 |>
        summariseSmd() |>
        mutate(variable_name = paste0(variable_name, "_at_index")) |>
        suppressMessages()
    ) |>
    bind(
      # smd at weights
      smdT |>
        summariseSmd() |>
        mutate(variable_name = paste0(variable_name, "_at_weight")) |>
        suppressMessages()
    ) |>
    splitStrata() |>
    mutate(time = tt) |>
    uniteStrata(cols = "time")
}
confoundingTime <- bind(confoundingTime) |>
  mutate(cdm_name = cdmName(cdm))

confoundingTime2 <- unbalanced0 |>
  group_by(across(everything())) |>
  tally(name = "count") |>
  ungroup() |>
  mutate(variable_name = "Unbalanced at index") |>
  union_all(
    unbalancedT |>
      group_by(across(everything())) |>
      tally(name = "count") |>
      ungroup() |>
      mutate(variable_name = "Unbalanced at weights")
  ) |>
  mutate(covariate = gsub("cov_", "", covariate))
concepts <- as.integer(unique(confoundingTime2$covariate))
nms <- cdm$concept |>
  filter(concept_id %in% concepts) |>
  select(concept_id, concept_name) |>
  collect() |>
  mutate(
    covariate = as.character(concept_id),
    variable_level = paste0(concept_name, " (", concept_id, ")")
  ) |>
  select("covariate", "variable_level")
confoundingTime2 <- confoundingTime2 |>
  inner_join(nms, by = "covariate") |>
  mutate(cdm_name = cdmName(cdm)) |>
  transformToSummarisedResult(
    group = c("weight_type", "reference", "comparator"), 
    estimates = "count"
  )

confoundingTime <- bind(confoundingTime, confoundingTime2)
