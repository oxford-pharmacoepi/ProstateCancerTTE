
assesConfounding <- c(0, 180, 360, 361, 540, 720)

confoundingTime <- weightTypes |>
  map(\(wt) {
    comparisons |>
      pmap(\(reference, exposed, comparison_id, comparison_name) {
        ind <- getWeights(comparison_id, wt) |>
          inner_join(followUp, by = c("cohort_name", "subject_id")) |>
          mutate(time_end = if_else(time_end > follow_up, follow_up, time_end)) |>
          filter(time_start < time_end) |>
          select(!"follow_up")
        assesConfounding |>
          map(\(t) {
            tryCatch({
              xt <- ind |>
                filter(time_start < t & t <= time_end) |>
                select(!c("time_start", "time_end")) |>
                inner_join(cohort, by = c("cohort_name", "subject_id")) |>
                createCovariatesMatrix(0, drugs, conditions, psa, gleason)
              
              if (length(unique(xt$cohort_name)) == 2) {
                smdCov <- xt |>
                  select("cohort_name", "subject_id", "weight", starts_with("cov_")) |>
                  pivot_longer(cols = starts_with("cov_"), names_to = "covariate", values_to = "value") |>
                  group_by(cohort_name, covariate) |>
                  summarise(prob = sum(value * weight) / sum(weight), .groups = "drop") |>
                  mutate(cohort_name = if_else(cohort_name == exposed, "pe", "pr")) |>
                  pivot_wider(names_from = "cohort_name", values_from = "prob", values_fill = 0) |>
                  mutate(
                    smd = if_else(pr == pe, 0, abs(pr - pe) /  sqrt((pe * (1 - pe) + pr * (1 - pr)) / 2))
                  ) |>
                  rename(mean_exposed = "pe", mean_reference = "pr")
                
                smdNum <- xt |>
                  select("cohort_name", "subject_id", "weight", "age", "index_year") |>
                  mutate(age = as.numeric(age), index_year = as.numeric(index_year)) |>
                  pivot_longer(cols = c("age", "index_year"), names_to = "covariate", values_to = "value") |>
                  group_by(cohort_name, covariate) |>
                  summarise(
                    mean = weighted.mean(value, weight),
                    var = weighted.var(value, weight),
                    .groups = "drop"
                  ) |>
                  mutate(cohort_name = if_else(cohort_name == exposed, "e", "r")) |>
                  pivot_longer(c("mean", "var")) |>
                  pivot_wider(names_from = c("name", "cohort_name"), values_from = "value", values_fill = 0) |>
                  mutate(
                    smd = abs(mean_e - mean_r) / sqrt((var_e + var_r) / 2)
                  ) |>
                  select("covariate", "mean_exposed" = "mean_e", "mean_reference" = "mean_r", "smd")
                
                smdCat <- xt |>
                  select("cohort_name", "subject_id", "weight", "psa", "gleason") |>
                  pivot_longer(cols = c("psa", "gleason"), names_to = "covariate", values_to = "value") |>
                  group_by(cohort_name, covariate) |>
                  mutate(denominator = sum(weight)) |>
                  ungroup() |>
                  mutate(covariate = paste0(covariate, " ", value)) |>
                  group_by(cohort_name, covariate, denominator) |>
                  summarise(prob = sum(weight), .groups = "drop") |>
                  mutate(
                    prob = prob / denominator,
                    cohort_name = if_else(cohort_name == exposed, "pe", "pr")
                  ) |>
                  select(!"denominator") |>
                  pivot_wider(names_from = "cohort_name", values_from = "prob", values_fill = 0) |>
                  mutate(
                    smd = if_else(pr == pe, 0, abs(pr - pe) /  sqrt((pe * (1 - pe) + pr * (1 - pr)) / 2))
                  ) |>
                  rename(mean_exposed = "pe", mean_reference = "pr")
                
                smd <- smdCov |>
                  union_all(smdCat) |>
                  union_all(smdNum)
                
                smdStats <- smd |>
                  summarise(
                    mean_smd = mean(smd),
                    min_smd = min(smd),
                    max_smd = max(smd),
                    sd_smd = sd(smd),
                    median_smd = median(smd),
                    q25_smd = quantile(smd, 0.25),
                    q75_smd = quantile(smd, 0.75),
                    unbalanced = sum(smd > 0.1)
                  ) |>
                  pivot_longer(everything()) |>
                  mutate(variable_name = "Standardised Mean Differences")
                
                unbalanced <- smd |>
                  filter(smd > 0.1) |>
                  pivot_longer(!"covariate") |>
                  rename(variable_name = "covariate")
                
                smdStats |>
                  union_all(unbalanced) |>
                  mutate(time = t)
              } else {
                NULL
              }
            },
            error = function(e) {
              cli_inform("Error in {wt}; {comparison_name}; {t}")
              NULL
            }
            )
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
  })

confoundingTime <- bind_rows(confoundingTime) |>
  mutate(cdm_name = cdmName(cdm))

concepts <- unique(confoundingTime$variable_name) |>
  keep(\(x) startsWith(x, "cov_")) |>
  str_remove("cov_") |>
  as.integer()
nms <- cdm$concept |>
  filter(concept_id %in% concepts) |>
  select(concept_id, concept_name) |>
  collect() |>
  mutate(
    variable_name = paste0("cov_", as.character(concept_id)),
    new_variable_name = paste0(concept_name, " (", concept_id, ")")
  ) |>
  select("variable_name", "new_variable_name")
confoundingTime <- confoundingTime |>
  left_join(nms, by = "variable_name") |>
  mutate(variable_name = coalesce(new_variable_name, variable_name)) |>
  select(!"new_variable_name")

results$confounding <- confoundingTime |>
  mutate(variable_level = sprintf("%.0f", time), result_type = "smd") |>
  select(!"time") |>
  pivot_wider() |>
  transformToSummarisedResult(
    group = c("comparison_id", "reference", "exposed"),
    strata = "weight_type",
    estimates = c("mean_smd", "min_smd", "max_smd", "sd_smd", "median_smd", "q25_smd", "q75_smd", "unbalanced", "mean_exposed", "mean_reference", "smd"),
    settings = "result_type"
  )
