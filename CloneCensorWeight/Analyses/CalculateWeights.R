# penalty factors
# weights calculation function (trimming)
# marginal <- xi |>
#   group_by(time_start) |>
#   summarise(marginal = weighted.mean(y, weight), .groups = "drop")

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
  mutate(
    follow_up = ceiling(follow_up/ti) * ti,
    follow_up_reason = if_else(follow_up > tmax, "censor", follow_up_reason),
    follow_up = if_else(follow_up > tmax, tmax, follow_up)
  ) |>
  arrange(cohort_name, subject_id)

covs <- colnames(x) |>
  (\(b) b[startsWith(b, "cov_")])()
forced <- c("age", "index_year", "psa", "gleason")

# Unweight analysis ----
weights$unweighted <- x |>
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
  "surveillance_4_months" = c("end_surveillance", "prostatectomy", "radiotheraphy"),
  "surveillance_6_months" = c("end_surveillance", "prostatectomy", "radiotheraphy"),
  "prostatectomy" = c("no prostatectomy", "radiotheraphy"),
  "radiotheraphy" = c("no radiotheraphy", "prostatectomy")
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
    select(all_of(forced), starts_with("cov_")) |>
    mutate(psa = as.numeric(psa), gleason = as.numeric(gleason)) |>
    as.matrix()
  y <- Surv(xi$follow_up, xi$status)
  pf <- c(rep(0, length(forced)), rep(1, length(covs)))
  fit <- cv.glmnet(X, y, family = "cox", alpha = 1, penalty.factor = pf)
  selected <- getSelected(fit)
  
  variables <- c(forced, selected)
  formula <- reformulate(variables, response = "Surv(follow_up, status)")
  cox <- coxph(formula, data = xi, x = TRUE)
  
  co[[nm]] <- broom::tidy(cox) |> 
    select(!c("statistic", "p.value")) |>
    rename(std_error = "std.error") |>
    mutate(cohort_name = nm, comparison_id = 0L)
  
  sv <- survfit(cox, newdata = xi)
  
  we[[nm]] <- times[times <= max(xi$follow_up)] |>
    map(\(time) {
      prob <- as.numeric(t(summary(sv, times = time, extend = TRUE)$surv))
      prob <- pmax(prob, quantile(prob, 0.01))
      tibble(subject_id = xi$subject_id, time_start = time, weight = 1 / prob)
    }) |>
    bind_rows() |>
    mutate(time_end = time_start + ti, cohort_name = nm, comparison_id = 0L)
  
  report()
}

weights$ipcw <- bind_rows(we) |>
  inner_join(
    x |>
      select("subject_id", "cohort_name", "follow_up"), 
    by = c("cohort_name", "subject_id")
  ) |>
  filter(time_start < follow_up) |>
  select(!"follow_up")
rm(we)
coef$ipcw <- bind_rows(co)
rm(co)

# IPTW at 360 ----

# only not censored people
x360 <- x |>
  filter(follow_up > 360)

co <- list()
we <- list()

for (i in comparisons$comparison_id) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPTW at 360 for ", cn))
  
  xi <- x360 |>
    filter(cohort_name %in% c(reference, exposed)) |>
    mutate(y = if_else(cohort_name == reference, 0, 1))
  
  if (sum(xi$y == 1) < 5 | sum(xi$y == 0) < 5) {
    next
  }
  
  # lasso variable selection
  X <- xi |> 
    select(all_of(forced), starts_with("cov_")) |>
    mutate(psa = as.numeric(psa), gleason = as.numeric(gleason)) |>
    as.matrix()
  y <- xi$y
  pf <- c(rep(0, length(forced)), rep(1, length(covs)))
  fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, penalty.factor = pf)
  selected <- getSelected(fit)
  
  variables <- c(forced, selected)
  formula <- reformulate(variables, response = "y")
  ps_model <- glm(
    formula,
    data = xi,
    family = binomial()
  )
  
  co[[i]] <- broom::tidy(ps_model) |> 
    select(!c("statistic", "p.value")) |>
    rename(std_error = "std.error") |>
    mutate(cohort_name = NA_character_, comparison_id = i)
  
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

weights$iptw360 <- x |>
  select("cohort_name", "subject_id") |>
  mutate(
    comparison_id = 0L,
    time_start = 0,
    time_end = 360,
    weight = 1
  ) |>
  union_all(
    bind_rows(we) |>
      mutate(time_start = 360, time_end = tmax)
  )
rm(we)
coef$iptw360 <- bind_rows(co)
rm(co)

# IPCW + IPTW360 ----
co <- list()
we <- list()
for (i in comparisons$comparison_id) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPTW at 360 with IPCW for ", cn))
  
  xi <- x |>
    filter(cohort_name %in% c(reference, exposed), follow_up > 360) |>
    mutate(y = if_else(cohort_name == reference, 0, 1)) |>
    left_join(
      weights$ipcw |>
        filter(time_start == 360) |>
        select("subject_id", "cohort_name", "weight"),
      by = c("subject_id", "cohort_name")
    )
  
  if (sum(xi$y == 1) < 5 | sum(xi$y == 0) < 5) {
    next
  }
  
  # lasso variable selection
  X <- xi |> 
    select(all_of(forced), starts_with("cov_")) |>
    mutate(psa = as.numeric(psa), gleason = as.numeric(gleason)) |>
    as.matrix()
  y <- xi$y
  pf <- c(rep(0, length(forced)), rep(1, length(covs)))
  fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, penalty.factor = pf, weights = xi$weight)
  selected <- getSelected(fit)
  
  variables <- c(forced, selected)
  formula <- reformulate(variables, response = "y")
  ps_model <- glm(
    formula,
    data = xi,
    family = binomial(), 
    weights = xi$weight
  )
  
  co[[i]] <- broom::tidy(ps_model) |> 
    mutate(cohort_name = NA_character_, comparison_id = i)
  
  ps <- predict(ps_model, newdata = xi, type = "response")
  marginal <- weighted.mean(xi$y, xi$weight)
  
  # needs trimming
  
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

weights$iptcw360 <- bind_rows(we) |>
  inner_join(
    weights$ipcw |>
      select("cohort_name", "subject_id", "time_start", "time_end", we_ipcw = "weight"),
    by = c("cohort_name", "subject_id"),
    relationship = "many-to-many"
  ) |>
  mutate(weight = if_else(time_start < 360, we_ipcw, we_iptw * we_ipcw)) |>
  select(!c("we_ipcw", "we_iptw"))
rm(we)
coef$iptcw360 <- bind_rows(co)
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
    mutate(y = if_else(cohort_name == reference, 0L, 1L), status = 1)
  
  # build long data set
  xi <- survSplit(
    Surv(follow_up, status) ~ .,
    data = xi,
    cut = sort(unique(xi$follow_up)),
    episode = "interval_id",
    start = "time_start",
    end = "time_end"
  )

  X <- model.matrix(
    reformulate(c("ns(time_start, df = 4)", forced, covs), intercept = FALSE),
    data = xi
  )
  
  # penalty for covs to use lasso as selection
  pf <- rep(0, ncol(X))
  pf[startsWith(colnames(X), "cov_")] <- 1
  
  fit <- cv.glmnet(
    X,
    xi$y,
    family = "binomial",
    alpha = 1,
    penalty.factor = pf,
    nfolds = 10
  )
  
  selected <- getSelected(fit)
  
  # Calculate ps with glm
  formula <- reformulate(c("ns(time_start, df = 4)", forced, selected), response = "y")
  fit <- glm(
    formula,
    data = xi,
    family = binomial()
  )
  ps <- predict(fit, newdata = xi, type = "response")
  
  co[[i]] <- broom::tidy(fit) |>
    mutate(comparison_id = i)
  
  # Calculate weights
  marginal <- xi |>
    group_by(time_start) |>
    summarise(marginal = mean(y), .groups = "drop")
  
  we[[i]] <- xi |>
    mutate(ps = ps) |>
    left_join(marginal, by = "time_start") |>
    mutate(
      weight = if_else(
        y == 1,
        marginal       / ps,
        (1 - marginal) / (1 - ps)
      ),
      comparison_id = i
    ) |>
    select("subject_id", "cohort_name", "time_start", "time_end", "weight", "comparison_id")
  
  report()
}

weights$iptw <- bind_rows(we)
rm(we)
coef$iptw <- bind_rows(co)
rm(co)

# IPCW + IPTW over time ----

co <- list()
we <- list()

for (i in seq_len(nrow(comparisons))) {
  reference <- comparisons$reference[comparisons$comparison_id == i]
  exposed <- comparisons$exposed[comparisons$comparison_id == i]
  cn <- comparisons$comparison_name[comparisons$comparison_id == i]
  
  recordTime(paste0("IPCW + IPTW over time for ", cn))
  
  # Build pooled person-time dataset
  xi <- x |>
    filter(cohort_name %in% c(reference, exposed)) |>
    mutate(y = if_else(cohort_name == reference, 0L, 1L), status = 1)
  
  # build long data set
  xi <- survSplit(
    Surv(follow_up, status) ~ .,
    data = xi,
    cut = sort(unique(xi$follow_up)),
    episode = "interval_id",
    start = "time_start",
    end = "time_end"
  ) |>
    left_join(
      weights$ipcw |>
        select("cohort_name", "subject_id", "time_start", "weight"),
      by = c("cohort_name", "subject_id", "time_start")
    )
  
  X <- model.matrix(
    reformulate(c("ns(time_start, df = 4)", forced, covs), intercept = FALSE),
    data = xi
  )
  
  # penalty for covs to use lasso as selection
  pf <- rep(0, ncol(X))
  pf[startsWith(colnames(X), "cov_")] <- 1
  
  fit <- cv.glmnet(
    X,
    xi$y,
    family = "binomial",
    alpha = 1,
    penalty.factor = pf,
    weight = xi$weight,
    nfolds = 10
  )
  
  selected <- getSelected(fit)
  
  # Calculate ps with glm
  formula <- reformulate(c("ns(time_start, df = 4)", forced, selected), response = "y")
  fit <- glm(
    formula,
    data = xi,
    family = binomial(),
    weight = xi$weight
  )
  ps <- predict(fit, newdata = xi, type = "response")
  
  co[[i]] <- broom::tidy(fit) |>
    mutate(comparison_id = i)
  
  # Calculate weights
  marginal <- xi |>
    group_by(time_start) |>
    summarise(marginal = weighted.mean(y, weight), .groups = "drop")
  
  we[[i]] <- xi |>
    mutate(ps = ps) |>
    left_join(marginal, by = "time_start") |>
    mutate(
      weight = if_else(
        y == 1,
        marginal       / ps * weight,
        (1 - marginal) / (1 - ps) * weight
      ),
      comparison_id = i
    ) |>
    select("subject_id", "cohort_name", "time_start", "time_end", "weight", "comparison_id")
  
  report()
}

weights$iptcw <- bind_rows(we)
rm(we)
coef$iptcw <- bind_rows(co)
rm(co)

# merge coefficients and prepare to export ----
coef <- bind_rows(coef, .id = "weight_type") 
concepts <- coef |>
  filter(startsWith(term, "cov_")) |>
  mutate(concept_id = as.numeric(gsub("cov_", "", term))) |>
  distinct(concept_id) |>
  pull()
concepts <- cdm$concept |>
  filter(concept_id %in% concepts) |>
  select(term = "concept_id", "concept_name") |>
  collect() |>
  mutate(
    concept_name = paste0(concept_name, " (", term, ")"),
    term = paste0("cov_", term)
  )
results$coef <- coef |>
  left_join(concepts, by = "term") |>
  mutate(
    cdm_name = cdmName(cdm),
    variable_name = coalesce(concept_name, term),
    variable_level = NA_character_,
    result_type = "coefficients"
  ) |>
  transformToSummarisedResult(
    group = c("comparison_id", "cohort_name"),
    additional = "weight_type",
    estimates = c("estimate", "std_error"),
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
  select("weight_type", "comparison_id", "cohort_name", "subject_id", "time_start", "time_end", "weight")
con <- weightsCon()
dbWriteTable(conn = con, name = "weights", value = weights, overwrite = TRUE)
rm(weights)
weights <- tbl(con, "weights")

weightTypes <- weights |>
  distinct(weight_type) |>
  pull()
