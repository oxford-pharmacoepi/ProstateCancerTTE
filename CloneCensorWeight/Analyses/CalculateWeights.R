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
  collect()
gleason <- cdm$gleason |>
  inner_join(
    cdm$my_cohort |>
      select(subject_id, index_date = cohort_start_date) |>
      distinct(),
    by = "subject_id"
  ) |>
  mutate(time = date_count_between(index_date, cohort_start_date)) |>
  select(subject_id, time, gleason_category) |>
  collect() 

# prepare subjects
cohort <- cdm$my_cohort |>
  addAgeQuery() |>
  mutate(index_year = get_year(cohort_start_date)) |>
  addCohortName() |>
  select(cohort_name, subject_id, follow_up, follow_up_reason, age, index_year) |>
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

# comparisons ----
cohorts <- unique(cohort$cohort_name)
comparisons <- expand_grid(
  reference = cohorts,
  exposed = cohorts
) |>
  filter(reference != exposed) |>
  mutate(
    comparison_id = row_number(),
    comparison_name = paste0(reference, " vs ", exposed)
  )
coef <- list()
weights <- list()

ti <- 10
tmax <- 1000
times <- seq(0, tmax - 1, by = ti)

# covariate matrix
x <- createCovariatesMatrix(cohort, 0, drugs, conditions, psa, gleason) |>
  mutate(follow_up = pmin(ceiling(follow_up/ti) * ti, tmax)) |>
  arrange(subject_id)

covs <- colnames(x) |>
  (\(b) b[startsWith(b, "cov_")])
forced <- c("age", "index_year", "psa", "gleason")

# Unweight analysis ----
weights$unweighted <- xi |>
  select("subject_id", "cohort_name") |>
  mutate(
    time_start = 0L,
    time_end = tmax,
    comparison_id = 0L,
    weight = 1
  )

# IPCW ----
artificialCensor <- list(
  "untreated" = c("prostatectomy", "radiotheraphy"),
  "surveillance_3_months" = c("end_surveillance", "prostatectomy", "radiotheraphy"),
  "surveillance_6_months" = c("end_surveillance", "prostatectomy", "radiotheraphy"),
  "prostatectomy" = c("no prostatectomy", "radiotheraphy"),
  "radiotheraphy" = c("no radiotheraphy", "prostatectomy")
)

# get censoring dates
x <- x |>
  mutate(
    follow_up_reason = if_else(follow_up > tmax, "censor", follow_up_reason)
  )

we <- list()
co <- list()
for (nm in names(artificialCensor)) {
  recordTime(paste0("IPCW for ", nm))
  
  reasons <- artificialCensor[[nm]]
  xi <- x |>
    filter(cohort_name == nm) |>
    mutate(status = if_else(follow_up_reason %in% reasons, 1, 0)) |>
    select(!c("cohort_name", "follow_up_reason"))
  
  # lasso variable selection
  X <- xi |> 
    select(starts_with("cov_")) |>
    as.matrix()
  y <- Surv(xi$follow_up, xi$status)
  fit <- cv.glmnet(X, y, family = "cox", alpha = 1)
  selected <- coef(fit, s = "lambda.min") |>
    (\(b) rownames(b)[b[, 1] != 0])()
  
  variables <- c("age", "index_year", "psa", "gleason", selected)
  formula <- reformulate(variables, response = "Surv(follow_up, status)")
  cox <- coxph(formula, data = xi, x = TRUE)
  
  co[[nm]] <- broom::tidy(cox) |> 
    mutate(cohort_name = nm, comparison_id = 0L)
  
  sv <- survfit(cox, newdata = xi)
  
  we[[nm]] <- times[times <= max(xi$follow_up)] |>
    map(\(time) {
      prob <- as.numeric(t(summary(sv, times = time, extend = TRUE)$surv))
      prob <- pmax(prob, quantile(prob, 0.01))
      tibble(subject_id = xi$subject_id, time = time, weight = 1 / prob)
    }) |>
    bind_rows() |>
    mutate(cohort_name = nm, comparison_id = 0L)
  
  report()
}

weights$ipcw <- bind_rows(we)
rm(we)
coef$ipcw <- bind_rows(co)
rm(co)

# IPTW at 365 ----

# only not censored people
x <- x |>
  filter(follow_up > 365)

co <- list()
we <- list()

for (i in comparisons$comparison_id) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPTW at 365 for ", cn))
  
  xi <- x |>
    filter(cohort_name %in% c(reference, exposed)) |>
    mutate(y = if_else(cohort_name == reference, 0, 1))
  
  if (sum(xi$y == 1) < 5 | sum(xi$y == 0) < 5) {
    next
  }
  
  # lasso variable selection
  X <- xi |> 
    select(starts_with("cov_")) |>
    as.matrix()
  y <- xi$y
  fit <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  selected <- coef(fit, s = "lambda.min") |>
    (\(b) rownames(b)[b[, 1] != 0])() |>
    keep(\(x) x != "(Intercept)")
  
  variables <- c("age", "index_year", "psa", "gleason", selected)
  formula <- reformulate(variables, response = "y")
  ps_model <- glm(
    formula,
    data = xi,
    family = binomial()
  )
  
  co[[i]] <- broom::tidy(ps_model) |> 
    mutate(reference = reference, exposed = exposed)
  
  ps <- predict(ps_model, newdata = xi, type = "response")
  marginal <- mean(xi$y)
  
  we[[i]] <- xi |>
    mutate(
      ps      = ps,
      weight = if_else(
        y == 1,
        marginal / ps,
        (1 - marginal) / (1 - ps)
      ),
      comparison_id = i
    ) |>
    select("subject_id", "cohort_name", "comparison_id", "weight")
  
  report()
}

weights$iptw365 <- bind_rows(we)
rm(we)
coef$iptw365 <- bind_rows(co)
rm(co)

# IPCW + IPTW365 ----

co <- list()
we <- list()

for (i in comparisons$comparison_id) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPTW at 365 with IPCW for ", cn))
  
  xi <- x |>
    filter(cohort_name %in% c(reference, exposed)) |>
    mutate(y = if_else(cohort_name == reference, 0, 1)) |>
    inner_join(
      weightsIPCW |>
        filter(cohort_name %in% c(reference, exposed), time == 370) |>
        select("subject_id", "weight"),
      by = "subject_id"
    )
  
  if (sum(xi$y == 1) < 5 | sum(xi$y == 0) < 5) {
    next
  }
  
  # lasso variable selection
  X <- xi |> 
    select(starts_with("cov_")) |>
    as.matrix()
  y <- xi$y
  fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, weights = xi$weight)
  selected <- coef(fit, s = "lambda.min") |>
    (\(b) rownames(b)[b[, 1] != 0])() |>
    keep(\(x) x != "(Intercept)")
  
  variables <- c("age", "index_year", "psa", "gleason", selected)
  formula <- reformulate(variables, response = "y")
  ps_model <- glm(
    formula,
    data = xi,
    family = binomial(), 
    weights = xi$weight
  )
  
  co[[i]] <- broom::tidy(ps_model) |> 
    mutate(reference = reference, exposed = exposed)
  
  ps <- predict(ps_model, newdata = xi, type = "response")
  marginal <- mean(xi$y)
  
  we[[i]] <- xi |>
    mutate(
      ps      = ps,
      we_iptw = if_else(
        y == 1,
        marginal / ps,
        (1 - marginal) / (1 - ps)
      ),
      comparison_id = i
    ) |>
    select("comparison_id", "cohort_name", "subject_id", "we_iptw")
  
  report()
}

weights$iptcw365 <- bind_rows(we) |>
  inner_join(
    weights$ipcw |>
      select("cohort_name", "subject_id", "time", we_ipcw = "weight"),
    by = c("cohort_name", "subject_id")
  ) |>
  mutate(weight = if_else(time <= 365, we_ipcw, we_iptw * we_ipcw)) |>
  select(!c("we_ipcw", "we_iptw"))
rm(we)
coef$iptcw365 <- bind_rows(co)
rm(co)

# IPTW over time ----

co <- list()
we <- list()

for (i in seq_len(nrow(comparisons))) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPTW over time for ", cn))
  
  # Build pooled person-time dataset
  xi <- x |>
    filter(cohort_name %in% c(reference, exposed)) |>
    mutate(y = if_else(cohort_name == reference, 0L, 1L))
  
  # build long data set
  xi <- survSplit(
    Surv(time, censor == "Censor") ~ .,
    data = xi,
    cut = sort(unique(xi$time)),
    episode = "interval_id",
    start = "time_start",
    end = "time_end"
  )

  X <- model.matrix(
    reformulate(c("ns(follow_up, df = 4)", forced, covs), intercept = FALSE),
    data = xi
  )
  
  # penalty for covs to use lasso as selection
  pf <- c(rep(0, length(forced) + 4), rep(1, length(covs)))
  
  fit <- cv.glmnet(
    X,
    xi$y,
    family = "binomial",
    alpha = 1,
    penalty.factor = pf,
    nfolds = 10
  )
  
  selected <- coef(fit, s = "lambda.min") |>
    (\(b) rownames(b)[b[, 1] != 0])() |>
    setdiff(c("(Intercept)", colnames(forced)))
  
  # Calculate ps with glm
  formula   <- reformulate(c("ns(follow_up, df = 4)", forced, selected), response = "y")
  fit <- glm(
    formula,
    data = xi,
    family = binomial()
  )
  ps <- predict(fit, newdata = xi, type = "response")
  
  co[[i]] <- broom::tidy(fit) |>
    mutate(comparison_id = i)
  
  # Calculate weights
  marginal_t <- xi |>
    group_by(time_end) |>
    summarise(marginal = weighted.mean(y, weight), .groups = "drop")
  
  we[[i]] <- xi |>
    mutate(ps = ps) |>
    left_join(marginal_t, by = "time_end") |>
    mutate(
      weight = if_else(
        y == 1,
        marginal       / ps,
        (1 - marginal) / (1 - ps)
      ),
      comparison = nm
    ) |>
    select(subject_id, cohort_name, time_start, time_end, weight, comparison)
  
  report()
}

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

# IPCW + IPTW over time ----

# merge coefficients and prepare to export ----
coef <- bind_rows(coef, .id = "weight_type") 
concepts <- coef |>
  filter(startsWith(variable, "cov_")) |>
  mutate(concept_id = as.numeric(gsub("cov_", "", variable))) |>
  distinct(concept_id) |>
  pull()
concepts <- cdm$concept |>
  filter(concept_id %in% concepts) |>
  select(variable = "concept_id", "concept_name") |>
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
    group = c("comparison_id", "cohort_name"),
    strata = "time",
    additional = "weight_type",
    estimates = "coef",
    settings = "result_type"
  )

# save weights ----
weights <- bind_rows(weights, .id = "weight_type") |>
  left_join(
    cohort |>
      select("cohort_name", "subject_id", "follow_up"),
    by = c("cohort_name", "subject_id")
  ) |>
  filter(time_start < follow_up) |>
  select("weight_type", "comparison_id", "cohort_name", "subject_id", "time", "weight")
con <- weightsCon()
dbWriteTable(conn = con, name = "weights", value = weights)
rm(weights)
weights <- tbl(con, "weights")
