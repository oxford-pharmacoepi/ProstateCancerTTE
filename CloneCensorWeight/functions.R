
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
  n_min <- floor(0.005 * nrow(cohort))
  
  # prepare psa
  x_psa <- psa |>
    filter(.data$time <= .env$time) |>
    group_by(subject_id) |>
    filter(.data$time == max(.data$time, na.rm = TRUE)) |>
    ungroup() |>
    select(subject_id, psa = psa_category)
  
  # prepare gleason
  x_gleason <- gleason |>
    filter(.data$time <= .env$time) |>
    group_by(subject_id) |>
    filter(.data$time == max(.data$time, na.rm = TRUE)) |>
    ungroup() |>
    select(subject_id, gleason = gleason_category)
  
  # drugs
  x_drugs <- drugs |>
    filter(.data$time <= .env$time & .data$time >= .env$time - 365) |>
    distinct(subject_id, covariate) |>
    mutate(value = 1)
  min_prev <- x_drugs |>
    group_by(covariate) |>
    tally() |>
    filter(n >= n_min) |>
    pull(covariate)
  x_drugs <- x_drugs |>
    filter(covariate %in% min_prev) |>
    pivot_wider(names_from = "covariate", values_from = "value")
  
  # conditions
  x_conditions <- conditions |>
    filter(.data$time <= .env$time) |>
    distinct(subject_id, covariate) |>
    mutate(value = 1)
  min_prev <- x_conditions |>
    group_by(covariate) |>
    tally() |>
    filter(n >= n_min) |>
    pull(covariate)
  x_conditions <- x_conditions |>
    filter(covariate %in% min_prev) |>
    pivot_wider(names_from = "covariate", values_from = "value")
  
  # prepare data
  cohort |>
    mutate(status = if_else(follow_up > time, 1, 0)) |>
    left_join(x_psa, by = "subject_id") |>
    left_join(x_gleason, by = "subject_id") |>
    left_join(x_conditions, by = "subject_id") |>
    left_join(x_drugs, by = "subject_id") |>
    mutate(across(starts_with("cov_"), \(x) coalesce(x, 0)))
}
modelWeights <- function(cohort, time) {
  coef <- tibble(
    variable = character(),
    coef = numeric(),
    cohort_name = character(),
    time = numeric()
  )
  weights <- tibble(
    subject_id = integer(),
    prob = numeric(),
    weight = numeric(),
    cohort_name = character(),
    time = numeric()
  )
  
  nms <- unique(cohort$cohort_name)
  for (nm in nms) {
    
    # filter cohort of interest
    x <- cohort |>
      filter(.data$cohort_name == .env$nm) |>
      select(!c("cohort_name", "follow_up"))
    
    # fit model
    res <- tryCatch(calculateWeights(x), error = function(e) as.character(e))
    
    if (is.character(res)) {
      cli_inform(c(x = "failed to fit model"))
      cli_inform(message = res)
      weights <- weights |>
        union_all(
          x |>
            distinct(subject_id) |>
            mutate(
              prob = 1,
              weight = 1,
              cohort_name = nm, 
              time = time
            )
        )
    } else {
      coef <- coef |>
        union_all(
          res$coef |>
            mutate(cohort_name = nm, time = time)
        )
      weights <- weights |>
        union_all(
          res$weights |>
            mutate(cohort_name = nm, time = time)
        )
    }
  }
  
  list(coef = coef, weights = weights)
}
calculateWeights <- function(x) {
  # lasso
  X <- x |>
    select(starts_with("cov_")) |>
    as.matrix()
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg <- cv.glmnet(x = X, y = x$status, lambda = lambdas, standardize = TRUE, nfolds = 5, alpha = 1)
  selected_cov <- coef(lasso_reg, s = lasso_reg$lambda.1se)[,1] |>
    keep(\(x) x != 0) |>
    names() |>
    keep(\(x) !grepl("Intercept", x))
  
  # regression
  X <- x |>
    select(!subject_id) |>
    mutate(
      missing_psa = if_else(is.na(psa), 1, 0),
      missing_gleason = if_else(is.na(gleason), 1, 0),
      psa = coalesce(psa, 0),
      gleason = coalesce(gleason, 0)
    )
  
  fit <- glm(status ~ ., data = X, family = binomial())
  
  # coefficients
  coeff <- fit |>
    coefficients() |>
    as_tibble(rownames = "variable") |>
    rename(coef = value)
  
  # save probabilities
  weights <- tibble(subject_id = x$subject_id, prob = predict(fit, type = "response")) |>
    mutate(
      prob = if_else(prob < 0.05, 0.05, prob),
      weight = 1 / prob
    )
  
  list(coeff = coeff, weights = weights)
}
characterisation <- function(cohort) {
  cohort <- cohort |>
    addTableIntersectField(
      tableName = "psa",
      field = "psa_category",
      window = c(-Inf, 0),
      order = "last",
      nameStyle = "last_psa"
    ) |>
    addTableIntersectField(
      tableName = "gleason",
      field = "gleason_category",
      window = c(-Inf, 0),
      order = "last",
      nameStyle = "last_gleason"
    ) |>
    mutate(across(c("last_psa", "last_gleason"), \(x) coalesce(x, "Unknown")))
  
  attrition <- summariseCohortAttrition(cohort = cohort)
  
  char <- summariseCharacteristics(
    cohort = cohort,
    cohortIntersectFlag = list(
      "Prior comorbidities in [-Inf, -1]" = list(
        targetCohortName = "conditions",
        window = c(-Inf, -1)
      ),
      "Prior medication in [-365, -1]" = list(
        targetCohortName = "medications",
        window = c(-365, -1)
      )
    ),
    cohortIntersectCount = list(
      "Number visists in [-365, -1]" = list(
        targetCohortName = "visits",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("last_psa", "last_gleason"), 
    estimates = list(other = c("count", "percentage"))
  )
  
  bind(attrition, char)
}
calculateSmd <- function(x) {
  labs <- x |>
    distinct(weight_type, reference, comparator, cohort_name)
  
  # all covariates
  x <- x |>
    full_join(
      x |>
        distinct(covariate) |>
        cross_join(labs),
      by = c("covariate", "reference", "comparator", "weight_type", "cohort_name")
    ) |>
    mutate(p = coalesce(p, 0))
  
  # calculate smd
  x |>
    mutate(cohort_name = if_else(cohort_name == reference, "pr", "pc")) |>
    pivot_wider(names_from = "cohort_name", values_from = "p") |>
    mutate(
      smd = if_else(pr == pc, 0, abs(pr - pc) /  sqrt((pc * (1 - pc) + pr * (1 - pr)) / 2)),
      unbalanced = if_else(smd > 0.1, 1, 0)
    )
}
summariseSmd <- function(x) {
  x |>
    summariseResult(
      group = list(c("weight_type", "reference", "comparator")),
      counts = FALSE, 
      variables = list("smd", "unbalanced"), 
      estimates = list(c("median", "mean", "max"), "count")
    )
}
summariseOutcomeModel <- function(weights, outcomes, cdmName) {
  
  weights <- weights |>
    dplyr::group_by(weight_type, reference, comparator) |>
    dplyr::group_split() |>
    as.list()
  
  outs <- outcomes |>
    distinct(outcome, outcome_type)
  
  resultHR <- list()
  resultSurv <- list()
  
  for (k in seq_len(nrow(outs))) {
    
    # outcomes
    outcome <- outs$outcome[k]
    outcomeType <- outs$outcome_type[k]
    
    cli_inform(c(i = "Fitting model for outcome: {.pkg {outcome}} ({outcomeType})"))
    
    outData <- outcomes |>
      filter(.data$outcome == .env$outcome) |>
      select(subject_id, out_time = time)
    
    res <- weights |>
      map(\(w) {
        data <- w |>
          left_join(
            outData, 
            by = "subject_id",
            relationship = "many-to-one"
          ) |>
          mutate(
            out_time = coalesce(out_time, 9999),
            time_end = if_else(out_time <= time_end, out_time, time_end),
            status = if_else(time_end == out_time, 1, 0)
          ) |>
          filter(time < time_end) |>
          select(!"out_time")
        
        wt <- unique(w$weight_type)
        ref <- unique(w$reference)
        comp <- unique(w$comparator)
        
        data <- data |>
          mutate(cohort_name = factor(cohort_name, c(ref, comp)))
        
        if (outcomeType == "main") {
          # fit survival model
          fit <- survfit(Surv(time, time_end, status) ~ cohort_name, 
                         data = data, 
                         weights = weight)
          
          # export survival probabilities
          summary_time <- sort(unique(c(0, data$time_end)))
          surv <- summary(fit, times = summary_time)
          surv <- tibble(
            weight_type = wt,
            reference = ref,
            comparator = comp,
            time = surv$time,
            survival = surv$surv,
            lower_survival = surv$lower,
            upper_survival = surv$upper,
            cohort_name = str_replace(surv$strata, "^cohort_name=", "")
          )
        }
        
        # fit cox model
        fit <- coxph(Surv(time, time_end, status) ~ cohort_name,
                     data = data,
                     weights = weight,
                     cluster = subject_id)
        
        # export hazard ratios
        hr <- summary(fit) |>
          coefficients() |>
          as_tibble(rownames = "comparator") |>
          mutate(reference = ref) |>
          rename("se_coef" = "se(coef)") |>
          mutate(comparator = str_replace(comparator, "cohort_name", "")) |>
          select(reference, comparator, coef, se_coef) |>
          mutate(weight_type = wt)
        
        if (outcomeType == "main") {
          list(hr = hr, surv = surv)
        } else {
          list(hr = hr)
        }
      })
    
    resultHR[[outcome]] <- res |>
      map("hr") |>
      bind_rows() |>
      mutate(outcome = outcome, outcome_type = outcomeType)
    
    if (outcomeType == "main") {
      resultSurv[[outcome]] <- res |>
        map("surv") |>
        bind_rows() |>
        mutate(outcome = outcome, outcome_type = outcomeType)
    }
  }
  
  # format results
  cli_inform(c(i = "Formatting results"))
  
  resultHR <- resultHR |>
    bind_rows() |>
    mutate(
      cdm_name = cdmName,
      variable_name = "Cox model",
      variable_level = NA_character_,
      result_type = "cox_regression",
      hr = exp(coef),
      hr_lower = exp(coef - 1.96 * se_coef),
      hr_upper = exp(coef + 1.96 * se_coef)
    ) |>
    transformToSummarisedResult(
      group = c("weight_type", "reference", "comparator"),
      strata = c("outcome_type", "outcome"),
      estimates = c("hr", "hr_lower", "hr_upper", "coef", "se_coef"),
      settings = "result_type"
    )
  
  resultSurv <- resultSurv |>
    bind_rows() |>
    mutate(
      cdm_name = cdmName,
      variable_name = "Survival probability",
      variable_level = sprintf("%i", time),
      result_type = "survival_probability"
    ) |>
    transformToSummarisedResult(
      group = c("weight_type", "reference", "comparator", "cohort_name"),
      strata = c("outcome_type", "outcome"),
      estimates = c("survival", "lower_survival", "upper_survival"),
      settings = "result_type"
    )
  
  bind(resultHR, resultSurv)
}
