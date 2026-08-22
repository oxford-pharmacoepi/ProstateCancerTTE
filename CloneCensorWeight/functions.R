
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
    mutate(
      psa = factor(coalesce(psa, "Missing"), levels = c("Missing", "[0, 3)", "[10, 20)", "[6, 10)", "[3, 6)", "[20, 40)", "[40, Inf)")),
      gleason = factor(coalesce(gleason, "Missing"), levels = c("Missing", "2 to 6", "7", "8 to 10"))
    ) |>
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
        targetCohortTable = "conditions",
        window = c(-Inf, -1)
      ),
      "Prior medication in [-365, -1]" = list(
        targetCohortTable = "medications",
        window = c(-365, -1)
      )
    ),
    cohortIntersectCount = list(
      "Number visists in [-365, -1]" = list(
        targetCohortTable = "visits",
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
    ) |>
    addResultType("smd")
}
summariseOutcomeModel <- function(weightTypes, outcomes, cdmName) {
  
  outs <- outcomes |>
    distinct(outcome, outcome_type)
  
  result <- weightTypes |>
    map(\(wt) {
      compResult <- comparisons |>
        pmap(\(reference, exposed, comparison_id, comparison_name) {
          ind <- getWeights(comparison_id, wt) |>
            inner_join(followUp, by = c("cohort_name", "subject_id")) |>
            mutate(time_end = if_else(time_end > follow_up, follow_up, time_end)) |>
            filter(time_start < time_end) |>
            select(!"follow_up")
          
          outResult <- outs |>
            pmap(\(outcome, outcome_type) {
              cli_inform(c(i = "Fitting model for: {wt}; {comparison_name}; {outcome}"))
              
              tryCatch({
                outData <- outcomes |>
                  filter(.data$outcome == .env$outcome) |>
                  select(subject_id, out_time = time)
                
                data <- ind |>
                  left_join(outData, by = "subject_id") |>
                  mutate(
                    out_time = coalesce(out_time, 9999),
                    time_end = if_else(out_time <= time_end, out_time, time_end),
                    status = if_else(time_end == out_time, 1, 0)
                  ) |>
                  filter(time_start < time_end) |>
                  select(!"out_time") |>
                  mutate(cohort_name = factor(cohort_name, c(reference, exposed)))
                
                if (outcome_type == "main") {
                  # fit survival model
                  fit <- survfit(Surv(time_start, time_end, status) ~ cohort_name, 
                                 data = data, 
                                 weights = weight)
                  
                  # export survival probabilities
                  summary_time <- sort(unique(c(0, data$time_end)))
                  surv <- summary(fit, times = summary_time)
                  surv <- tibble(
                    weight_type = wt,
                    reference = reference,
                    exposed = exposed,
                    comparison_id = comparison_id,
                    time = surv$time,
                    survival = surv$surv,
                    lower_survival = surv$lower,
                    upper_survival = surv$upper,
                    cohort_name = str_replace(surv$strata, "^cohort_name=", ""),
                    outcome = outcome,
                    outcome_type = outcome_type
                  )
                } else {
                  surv <- NULL
                }
                
                # fit cox model
                fit <- coxph(Surv(time_start, time_end, status) ~ cohort_name,
                             data = data,
                             weights = weight,
                             cluster = subject_id)
                
                # export hazard ratios
                hr <- summary(fit) |>
                  coefficients() |>
                  as_tibble(rownames = "exposed") |>
                  mutate(reference = reference) |>
                  rename("se_coef" = "se(coef)") |>
                  mutate(exposed = str_replace(exposed, "cohort_name", "")) |>
                  select("reference", "exposed", "coef", "se_coef") |>
                  mutate(
                    comparison_id = comparison_id,
                    weight_type = wt,
                    outcome = outcome,
                    outcome_type = outcome_type
                  )
                
                list(hr = hr, surv = surv)
              },
              error = function(e) {
                cli_inform(c(x = "ERROR!", as.character(e)))
                list(hr = NULL, surv = NULL)
              })
              
            })
          
          list(
            hr = map(outResult, "hr") |>
              bind_rows(),
            surv = map(outResult, "surv") |>
              bind_rows()
          )
        })
      
      list(
        hr = map(compResult, "hr") |>
          bind_rows(),
        surv = map(compResult, "surv") |>
          bind_rows()
      )
    })

  # format results
  cli_inform(c(i = "Formatting results"))
  
  resultHR <- map(result, "hr") |>
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
      group = c("weight_type", "reference", "exposed", "comparison_id"),
      strata = c("outcome_type", "outcome"),
      estimates = c("hr", "hr_lower", "hr_upper", "coef", "se_coef"),
      settings = "result_type"
    )
  
  resultSurv <- map(result, "surv") |>
    bind_rows() |>
    mutate(
      cdm_name = cdmName,
      variable_name = "Survival probability",
      variable_level = sprintf("%.0f", .data$time),
      result_type = "survival_probability"
    ) |>
    transformToSummarisedResult(
      group = c("weight_type", "reference", "exposed", "comparison_id", "cohort_name"),
      strata = c("outcome_type", "outcome"),
      estimates = c("survival", "lower_survival", "upper_survival"),
      settings = "result_type"
    )
  
  bind(resultHR, resultSurv)
}
addResultType <- function(result, resultType) {
  result |>
    newSummarisedResult(
      settings = settings(result) |>
        mutate(result_type = .env$resultType)
    )
}
weightsCon <- function() {
  dbConnect(drv = duckdb(dbdir = here("data", "weights.duckdb")))
}
recordTime <- function(task) {
  time <- Sys.time()
  message <- paste0("Calculating ", task)
  logMessage(message = message)
  options(og.task = task)
  options(og.time = time)
}
report <- function() {
  ts <- getOption("og.time")
  task <- getOption("og.task")
  time <- Sys.time()
  diff <- round(as.numeric(difftime(time1 = time, time2 = ts, units = "secs")))
  diff <- sprintf("%ih %02im %02is", diff %/% 3600, diff %/% 60, diff %% 60)
  message <- paste0("Finished ", task, " in ", diff)
  logMessage(message = message)
}
getSelected <- function(fit) {
  coef(fit, s = "lambda.min") |>
    (\(b) rownames(b)[b[, 1] != 0])() |>
    keep(\(x) startsWith(x, "cov_"))
}
getWeights <- function(comparisonId, weightType) {
  cohorts <- comparisons |>
    filter(comparison_id == comparisonId) |>
    select("exposed", "reference") |>
    pivot_longer(everything()) |>
    pull("value")
  weights |>
    filter(weight_type == weightType) |>
    filter(cohort_name %in% cohorts) |>
    filter(comparison_id %in% c(0, comparisonId)) |>
    select("cohort_name", "subject_id", "time_start", "time_end", "weight") |>
    collect()
}
