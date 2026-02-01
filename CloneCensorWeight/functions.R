
extractCovariates <- function(table, individuals, total_ind, minFrequency, excludeCodes) {
  cdm <- cdmReference(individuals)
  id <- omopColumns(table = table, field = "standard_concept")
  date <- omopColumns(table = table, field = "start_date")
  
  allCov <- cdm[[table]] |>
    rename(concept_id = all_of(id), date = all_of(date)) |>
    inner_join(individuals, by = "person_id") |>
    mutate(time = date_count_between(cohort_start_date, date)) |>
    filter(time <= max_censor) |>
    select(person_id, time, concept_id) |>
    compute(name = "all_cov")
  covOfInterest <- allCov |>
    group_by(concept_id) |>
    summarise(n_ind = n_distinct(person_id)) |>
    mutate(percentage = n_ind / total_ind) |>
    filter(!concept_id %in% excludeCodes & percentage >= minFrequency) |>
    compute(name = "selected_cov")
  allCov <- allCov |>
    inner_join(
      covOfInterest |>
        select(concept_id),
      by = "concept_id"
    ) |>
    mutate(covariate = paste0("cov_", concept_id), table = table) |>
    distinct(person_id, time, covariate, table) |>
    collect() |>
    rename(subject_id = person_id)
  dropSourceTable(cdm = cdm, name = c("selected_cov", "all_cov"))
  allCov
}
createCovariatesMatrix <- function(cohort, time, drugs, conditions, psa, gleason) {
  # prepare psa
  psa <- psa |>
    filter(.data$time <= .env$time) |>
    group_by(subject_id) |>
    filter(.data$time == max(.data$time, na.rm = TRUE)) |>
    ungroup() |>
    select(subject_id, psa = psa_category)
  # prepare gleason
  gleason <- gleason |>
    filter(.data$time <= .env$time) |>
    group_by(subject_id) |>
    filter(.data$time == max(.data$time, na.rm = TRUE)) |>
    ungroup() |>
    select(subject_id, gleason = gleason_category)
  # drugs
  drugs <- drugs |>
    filter(.data$time <= .env$time & .data$time >= .env$time - 365) |>
    distinct(subejct_id, concept_id) |>
    mutate(value = 1) |>
    pivot_wider(names_from = "concept_id", values_from = "value")
  # conditions
  conditions <- conditions |>
    filter(.data$time <= .env$time) |>
    distinct(subejct_id, concept_id) |>
    mutate(value = 1) |>
    pivot_wider(names_from = "concept_id", values_from = "value")
  # prepare data
  cohort <- cohort |>
    mutate(status = if_else(followup > time, 1, 0)) |>
    left_join(psa, by = "subject_id") |>
    left_join(gleason, by = "subject_id") |>
    left_join(conditions, by = "subject_id") |>
    left_join(drugs, by = "subject_id") |>
    mutate(across(starts_with("cov_"), \(x) coalesce(x, 0)))
  
  coef <- list()
  weights <- list()
  
  nms <- unique(cohort$cohort_name)
  for (nm in nms) {
    cli_inform(c(i = "Calculating weights at time {.pkg time} and cohort: {.pkg nms}"))
    
    # filter cohort of interest
    x <- cohort |>
      filter(.data$cohort_name == .env$nm) |>
      select("subject_id", "status", starts_with("cov_"))
    
    # fit model
    res <- tryCatch(calculateWeights(x), error = function(e) as.character(e))
    
    if (is.character(res)) {
      cli_inform(c(x = "failed to fit model"))
      cli_inform(message = res)
    } else {
      coef <- append(coef, list(res$coef))
      weights <- append(weights, list(res$weights))
    }
  }
  
}
calculateWeights <- function(x) {
  # lasso
  X <- x |>
    select(!c("subject_id", "status")) |>
    as.matrix()
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg <- cv.glmnet(x = X, y = x$status, lambda = lambdas, standardize = TRUE, nfolds = 5, alpha = 1)
  selected_cov <- coef(lasso_reg, s = lasso_reg$lambda.1se) |>
    map(\(x) {
      x <- names(x[(x[,1]!=0),1])
      x[x != "(Intercept)"]
    }) |>
    unlist(use.names = FALSE) |>
    unique()
  
  # regression
  X <- x |>
    select(all_of(c("year_of_birth", "index_year", selected_cov))) |>
    as.matrix()
  fit <- glmnet(x = X, y = x$status, lambda = 0)
  
  # coefficients
  coeff <- fit |>
    coefficients() |>
    as_tibble(rownames = "status") |>
    pivot_longer(!"status", names_to = "covariate", values_to = "value")
  
  # save probabilities
  probs <- predict(fit, type = "probs")
  probabilities[[as.character(wti)]] <- xi |>
    select("cohort_name", "subject_id") |>
    bind_cols(as_tibble(probs))
  
  list(coeff = coeff, weights = weights)
}

calculateWeights <- function(x, conditions, exposures, min_frequency) {
  x <- x |>
    select("cohort_name", "subject_id", "time" = "censor_time", "age")

  sep <- 10
  weights_time <- seq(from = 0, to = 730, by = sep)

  coefficients <- list()
  probabilities <- list()
  weights <- list()

  for (wti in weights_time) {
    logMessage(paste0("Calculating weights at time: ", wti))

    xi <- x |>
      filter(time > wti)

    subjects <- unique(xi$subject_id)

    min_counts_i <- length(subjects) * min_frequency

    # exposures
    exps_i <- exposures |>
      filter(subject_id %in% subjects) |>
      group_by(concept_id) |>
      tally() |>
      filter(n >= min_counts_i) |>
      pull(concept_id)
    exposures_i <- exposures |>
      filter(concept_id %in% exps_i) |>
      mutate(value = 1, concept_id = paste0("cov_", concept_id)) |>
      pivot_wider(names_from = "concept_id", values_from = "value")

    # conditions
    cond_i <- conditions |>
      filter(subject_id %in% subjects) |>
      group_by(concept_id) |>
      tally() |>
      filter(n >= min_counts_i) |>
      pull(concept_id)
    conditions_i <- conditions |>
      filter(concept_id %in% cond_i) |>
      mutate(value = 1, concept_id = paste0("cov_", concept_id)) |>
      pivot_wider(names_from = "concept_id", values_from = "value")

    # create matrix
    covs <- paste0("cov_", c(cond_i, exps_i))
    xi <- xi |>
      left_join(conditions_i, by = "subject_id") |>
      left_join(exposures_i, by = "subject_id") |>
      mutate(across(all_of(covs), \(x) coalesce(x, 0)))

    # lasso to select variables
    X <- xi |>
      select(!c("cohort_name", "subject_id", "time", "age")) |>
      as.matrix()
    y <- xi$cohort_name
    lambdas <- 10^seq(2, -3, by = -.1)
    lasso_reg <- cv.glmnet(x = X, y = y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "multinomial", alpha = 1)
    selected_cov <- coef(lasso_reg, s = lasso_reg$lambda.1se) |>
      map(\(x) {
        x <- names(x[(x[,1]!=0),1])
        x[x != "(Intercept)"]
      }) |>
      unlist(use.names = FALSE) |>
      unique()

    # logistic regression
    X <- xi |>
      select(all_of(c("cohort_name", "age", selected_cov)))
    fit <- multinom(cohort_name ~ ., data = X, trace = FALSE, MaxNWts = 5000)

    # save model
    coefficients[[as.character(wti)]] <- fit |>
      coefficients() |>
      as_tibble(rownames = "group") |>
      pivot_longer(!"group", names_to = "covariate", values_to = "value")

    # save probabilities
    probs <- predict(fit, type = "probs")
    probabilities[[as.character(wti)]] <- xi |>
      select("cohort_name", "subject_id") |>
      bind_cols(as_tibble(probs))

    # # save weights
    # weights[[as.character(wti)]] <- probabilities[[as.character(wti)]] |>
    #   mutate(weights = case_when(
    #     cohort_name == "surveillance" ~ 1 - surveillance,
    #     cohort_name == "prostatectomy" ~ 1 - prostatectomy,
    #     cohort_name == "radiotheraphy" ~ 1 - radiotheraphy,
    #   )) |>
    #   select("cohort_name", "subject_id", "weights")
  }

  list(probabilities = probabilities, coefficients = coefficients)
}
summaryOutcome <- function(x, outcome, weights) {
  logMessage(paste0("Outcome model for: ", outcome))
  xo <- x |>
    rename(outcome = all_of(outcome)) |>
    mutate(
      status = if_else(is.na(outcome) | outcome > censor_time, 0, 1),
      time = if_else(status == 0, censor_time, outcome),
      reason = if_else(status == 0, censor_reason, "outcome")
    ) |>
    select("cohort_name", "subject_id", "status", "time", "reason", "age")

  probabilities <- weights$probabilities
  coefficients <- weights$coefficients
  sep <- unique(diff(as.numeric(names(probabilities))))

  # survival over time
  surv_data <- probabilities |>
    bind_rows(.id = "time_start") |>
    mutate(
      weight = case_when(
        cohort_name == "surveillance" ~ 1 - surveillance,
        cohort_name == "prostatectomy" ~ 1 - prostatectomy,
        cohort_name == "radiotheraphy" ~ 1 - radiotheraphy
      ),
      time_start = as.numeric(time_start),
      time_end = time_start + sep
    ) |>
    inner_join(
      xo |>
        select(!"age"),
      by = c("cohort_name", "subject_id")
    ) |>
    mutate(
      status = if_else(time_start < time & time <= time_end, status, 0),
      time_end = if_else(time_start < time & time <= time_end, time, time_end)
    ) |>
    select(!c("surveillance", "prostatectomy", "radiotheraphy"))
  fit <- survfit(Surv(time_start, time_end, status) ~ cohort_name, data = surv_data, weights = weight)
  summary_time <- c(0:max(surv_data$time_end))
  survival_summary <- summary(fit, times = summary_time)
  survival_summary <- tibble(
    time = survival_summary$time,
    survival = survival_summary$surv,
    lower_survival = survival_summary$lower,
    upper_survival = survival_summary$upper,
    cohort_name = str_replace(survival_summary$strata, "^cohort_name=", "")
  ) |>
    mutate(outcome = outcome) |>
    left_join(
      summary_time |>
        map_df(\(st) {
          lw <- if_else(st == 0, 0.01, st)
          surv_data |>
            filter(time_start < lw &  st <= time_end) |>
            group_by(cohort_name) |>
            summarise(
              number_subjects = n(),
              number_weighted_subjects = sum(weight),
              .groups = "drop"
            ) |>
            mutate(time = st)
        }),
      by = c("time", "cohort_name")
    )

  # hazard ratio
  surv_data <- surv_data |>
    group_by(cohort_name, subject_id) |>
    mutate(id = cur_group_id()) |>
    ungroup()
  hr_summary <- list("overall" = c(0, Inf), "first year" = c(0, 365), "second year" = c(366, 730)) |>
    map(\(interval) {
      data <- surv_data |>
        filter(interval[1] <= time_end & time_start <= interval[2]) |>
        mutate(
          time_start = if_else(time_start <= interval[1], interval[1], time_start),
          time_end = if_else(interval[2] <= time_end, interval[2], time_end),
          status = if_else(time == time_end, status, 0)
        ) |>
        filter(time_start < time_end)
      fit1 <- coxph(Surv(time_start, time_end, status) ~ cohort_name,
                    data = data,
                    weights = weight,
                    cluster = id)
      fit2 <- coxph(Surv(time_start, time_end, status) ~ cohort_name,
                    data = data |>
                      filter(cohort_name != "surveillance") |>
                      mutate(cohort_name = factor(cohort_name, levels = c("prostatectomy", "radiotheraphy"))),
                    weights = weight,
                    cluster = id)
      summary(fit1) |>
        coefficients() |>
        as_tibble(rownames = "comparator") |>
        mutate(reference = "surveillance") |>
        bind_rows(
          summary(fit2) |>
            coefficients() |>
            as_tibble(rownames = "comparator") |>
            mutate(reference = "prostatectomy")
        ) |>
        rename(
          "hazard_ratio" = "exp(coef)",
          "se_coef" = "se(coef)",
          #"se_coef_robust" = "robust se",
          "p_value" = "Pr(>|z|)"
        ) |>
        mutate(comparator = str_replace(comparator, "cohort_name", ""))
    }) |>
    bind_rows(.id = "interval") |>
    mutate(outcome = outcome) |>
    relocate("interval", "comparator", "reference", "outcome")

  # number events
  events <- surv_data |>
    group_by(time_start, cohort_name) |>
    summarise(
      number_events = sum(status),
      number_weighted_events = sum(status * weight),
      .groups = "drop"
    ) |>
    mutate(time_end = time_start + sep, outcome = outcome)

  # followup summary
  followup_summary <- xo |>
    group_by(cohort_name) |>
    summarise(
      n = n(),
      min = min(time),
      q05 = quantile(time, 0.05),
      q25 = quantile(time, 0.25),
      median = median(time),
      q75 = quantile(time, 0.75),
      q95 = quantile(time, 0.95),
      max = max(time),
      .groups = "drop"
    ) |>
    mutate(reason = "overall") |>
    relocate("cohort_name", "reason") |>
    union_all(
      xo |>
        group_by(cohort_name, reason) |>
        summarise(
          n = n(),
          min = min(time),
          q05 = quantile(time, 0.05),
          q25 = quantile(time, 0.25),
          median = median(time),
          q75 = quantile(time, 0.75),
          q95 = quantile(time, 0.95),
          max = max(time),
          .groups = "drop"
        )
    ) |>
    mutate(outcome = outcome)

  # export coefficients
  coefficients <- coefficients |>
    bind_rows(.id = "time_start") |>
    mutate(time_start = as.numeric(time_start), outcome = outcome)

  # export probabilities
  hist_sep <- 0.01
  breaks <- seq(from = 0, to = 1, by = hist_sep)
  labels <- as.character(breaks[-1] - hist_sep/2)
  probabilities <- probabilities |>
    map(\(x) {
      x |>
        pivot_longer(
          cols = c("surveillance", "prostatectomy", "radiotheraphy"),
          names_to = "prob_label",
          values_to = "prob"
        ) |>
        select(!"subject_id")
    }) |>
    bind_rows(.id = "time_start") |>
    mutate(prob_bin = as.character(cut(x = prob, breaks = breaks, labels = labels))) |>
    group_by(time_start, cohort_name, prob_label, prob_bin) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(time_start, cohort_name, prob_label) |>
    mutate(freq = n / sum(n)) |>
    ungroup() |>
    select(!"n") |>
    full_join(
      expand_grid(
        time_start = names(probabilities),
        cohort_name = c("surveillance", "prostatectomy", "radiotheraphy"),
        prob_label = c("surveillance", "prostatectomy", "radiotheraphy"),
        prob_bin = labels
      ),
      by = c("time_start", "cohort_name", "prob_label", "prob_bin")
    ) |>
    mutate(
      freq = coalesce(freq, 0),
      time_start = as.numeric(time_start),
      prob_bin = as.numeric(prob_bin),
      outcome = outcome
    ) |>
    arrange(time_start, cohort_name, prob_label, prob_bin)

  list(
    hr_summary = hr_summary,
    survival_summary = survival_summary,
    events = events,
    followup_summary = followup_summary,
    coefficients = coefficients,
    probabilities = probabilities
  )
}
bindResults <- function(result, cdmName) {
  group <- list(
    "hr_summary" = c("comparator", "reference"),
    "survival_summary" = "cohort_name",
    "events" = "cohort_name",
    "followup_summary" = "cohort_name",
    "coefficients" = "group",
    "probabilities" = c("cohort_name", "outcome")
  )
  strata <- list(
    "hr_summary" = "outcome",
    "survival_summary" = "outcome",
    "events" = "outcome",
    "followup_summary" = "outcome",
    "coefficients" = "outcome",
    "probabilities" = c("prob_bin", "prob_label")
  )
  additional <- list(
    "hr_summary" = "interval",
    "survival_summary" = "time",
    "events" = c("time_start", "time_end"),
    "followup_summary" = "reason",
    "coefficients" = c("covariate", "time_start"),
    "probabilities" = c("time_start")
  )
  estimates <- list(
    "hr_summary" = c("coef", "hazard_ratio", "se_coef", "z", "p_value"), # se_coef_robust
    "survival_summary" = c("survival", "lower_survival", "upper_survival", "number_subjects", "number_weighted_subjects"),
    "events" = c("number_events", "number_weighted_events"),
    "followup_summary" = c("n", "min", "q05", "q25", "median", "q75", "q95", "max"),
    "coefficients" = "value",
    "probabilities" = c("freq")
  )
  names(result[[1]]) |>
    map(\(nm) {
      x <- result |>
        map(\(x) x[[nm]]) |>
        bind_rows() |>
        mutate(
          result_type = nm,
          cdm_name = cdmName
        ) |>
        transformToSummarisedResult(
          group = group[[nm]], strata = strata[[nm]], additional = additional[[nm]],
          estimates = estimates[[nm]], settings = "result_type"
        )
    }) |>
    bind()
}
