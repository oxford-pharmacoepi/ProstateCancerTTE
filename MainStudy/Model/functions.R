### propensity scores ----

longDataFromCohort <- function(cdm, cohort_name = "optima_pc_rwd", excluded_codes){
  cohort_name_long <- paste(cohort_name, "long", sep = "_")

  cond_tmp_name <- paste0(cohort_name_long, "_cond_tmp")
  drug_tmp_name <- paste0(cohort_name_long, "_drug_tmp")


  cond_branch <- cdm[[cohort_name]] |>
    dplyr::inner_join(
      cdm$condition_occurrence |> dplyr::select(person_id, condition_concept_id, condition_start_date),
      by = c("subject_id" = "person_id")
    ) |>
    dplyr::filter(.data$condition_start_date <= .data$cohort_start_date) |>
    dplyr::mutate(
      days_diff = as.integer(cohort_start_date - condition_start_date),
      time_window = dplyr::if_else(days_diff < 366, "365_0", "inf_366")
    ) |>
    dplyr::rename(concept_id = condition_concept_id) |>
    dplyr::select(-condition_start_date, -days_diff) |>
    dplyr::compute(name = cond_tmp_name, temporary = TRUE)

  drug_branch <- cdm[[cohort_name]] |>
    dplyr::inner_join(
      cdm$drug_exposure |> dplyr::select(person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date),
      by = c("subject_id" = "person_id")
    ) |>
    dplyr::mutate(
      days_diff_start = as.integer(.data$cohort_start_date - .data$drug_exposure_start_date),
      days_diff_end   = as.integer(.data$cohort_start_date - .data$drug_exposure_end_date)
    ) |>
    dplyr::filter(
      (days_diff_start <= 365 & days_diff_start > 0) |
        (days_diff_end <= 365 & days_diff_end > 0) |
        (.data$cohort_start_date > .data$drug_exposure_start_date & .data$cohort_start_date < .data$drug_exposure_end_date)
    ) |>
    dplyr::rename(concept_id = drug_concept_id) |>
    dplyr::mutate(time_window = "365_0") |>
    dplyr::select(-drug_exposure_start_date, -drug_exposure_end_date, -days_diff_start, -days_diff_end) |>
    dplyr::compute(name = drug_tmp_name, temporary = TRUE)


  cdm[[cohort_name_long]] <- dplyr::union_all(cond_branch, drug_branch) |>
    dplyr::filter(!(.data$concept_id %in% excluded_codes)) |>
    dplyr::mutate(concept_id = paste(.data$concept_id, .data$time_window, sep = "_")) |>
    dplyr::select(-time_window) |>
    dplyr::compute(name = cohort_name_long, temporary = FALSE)

  long_cols <- colnames(cdm[[cohort_name_long]])


  missing_rows <- cdm[[cohort_name]] |>
    dplyr::anti_join(
      cdm[[cohort_name_long]] |> dplyr::select(subject_id) |> dplyr::distinct(),
      by = "subject_id"
    ) |>
    dplyr::mutate(concept_id = NA_character_) |>
    dplyr::select(dplyr::all_of(long_cols))

  # union existing long rows with the new "missing" rows
  cdm[[cohort_name_long]] <- dplyr::union_all(
    cdm[[cohort_name_long]],
    missing_rows
  ) |>
    dplyr::compute(name = cohort_name_long)

return(cdm)
}

getFrequentConcepts <- function(cohort){
  n <- cohort |>
    dplyr::distinct(subject_id) |>
    dplyr::tally() |>
    dplyr::pull()|>
    as.numeric()

  frequent_concepts <- cohort |>
    dplyr::filter(!is.na(concept_id)) |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::summarise(n_subjects = (dplyr::n_distinct(.data$subject_id)), .groups = "drop") |>
    dplyr::mutate(perc = .data$n_subjects / .env$n) |>
    dplyr::filter(perc >= 0.005)|> dplyr::pull(concept_id)

return(frequent_concepts)
}

visitsCount <- function(cdm, cohort_name = "optima_pc_rwd"){
  cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
  cdm[[cohort_name_visits]] <- cdm[[cohort_name]] |>
    dplyr::inner_join(cdm$visit_occurrence |>
                       dplyr::filter(!(.data$visit_concept_id %in%  c(38004268, 38004269))) |>
                       dplyr::select("person_id", "visit_concept_id", "visit_start_date", "visit_end_date"),
                     by = c("subject_id" = "person_id")) |>
    dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$visit_start_date),
                  days_diff_end = as.integer(.data$cohort_start_date - .data$visit_end_date)) |>
    dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start >= 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end >= 0 ) | (.data$cohort_start_date > .data$visit_start_date & .data$cohort_start_date < .data$visit_end_date)) |>
    dplyr::group_by(.data$subject_id, .data$visit_concept_id, .data$cohort_definition_id, .data$cohort_start_date, .data$cohort_end_date) |>
    dplyr::tally() |>
    tidyr::pivot_wider(names_from = "visit_concept_id", values_from = "n", names_prefix = "visit", values_fill = list(n = 0)) |>
    dplyr::rename("visit581477_365_0" = "visit581477", "visit9201_365_0" = "visit9201")|>
    dplyr::compute(name = cohort_name_visits)

  cdm[[cohort_name_visits]] <- cdm[[cohort_name_visits]] |>
    dplyr::right_join(cdm[[cohort_name]]) |>
    dplyr::mutate(visit581477_365_0 = dplyr::coalesce(.data$visit581477_365_0, 0),
                  visit9201_365_0 = dplyr::coalesce(.data$visit9201_365_0, 0)) |>
    dplyr::compute(name = cohort_name_visits)
  return(cdm)
}
getWideData <- function(cohort, frequent_concepts, visits) {
  id_vars <- c(
    "cohort_definition_id","subject_id","cohort_start_date","cohort_end_date",
    "latest_gleason_score_value","latest_n_status","latest_psa_value", "latest_t_status", "psa_value",
    "age","age_group","source"     # drop if not present
  )
  wide_data <- cohort |>
    dplyr::filter(concept_id %in% frequent_concepts) |>
    dplyr::distinct() |>
    dplyr::mutate(value = 1) |>
    tidyr::pivot_wider(
    names_from = concept_id,
    values_from = value,
    values_fill = list(value = 0)
  )|>
    dplyr::left_join(visits) |>
    dplyr::collect() |>
    dplyr::mutate(year = clock::get_year(cohort_start_date)) |>
    dplyr::mutate(year_group = dplyr::case_when(
      year < 2010 ~ "<2010",
      year >= 2010 & year < 2015 ~ "2010-2015",
      year >= 2015 & year < 2020 ~ "2015-2020",
      year >= 2020 ~ ">2020",
      TRUE ~ NA_character_
      ))

  return(wide_data)
}

addVariables <- function(cohort) {
  cdm <- omopgenerics::cdmReference(cohort)
  cohort_name <- omopgenerics::tableName(cohort)

  if (grepl("rwd", cohort_name)){

    gleason_cohort <- "gleason_rwd"

    n_status_cohort <- "n_status_rwd"

    t_status_cohort <- "t_status_rwd"

    psa_cohort <- "psa_values_rwd"


  } else if (grepl("trial", cohort_name)) {
    gleason_cohort <- "gleason_trial"

    n_status_cohort <- "n_status_trial"

    t_status_cohort <- "t_status_trial"

    psa_cohort <- "psa_values_trial"
  }

  cols <- colnames(cohort)
  if (!("latest_gleason_score_value" %in% cols)) {
  cohort <- cohort |>
    dplyr::left_join(cdm[[gleason_cohort]] |> dplyr::select("subject_id","latest_gleason_score_value" ),  by = "subject_id") |>
    dplyr::mutate(
      latest_gleason_score_value = dplyr::coalesce(.data$latest_gleason_score_value, "missing")
    ) |>
    dplyr::compute(name = cohort_name)
  }
  if (!("latest_n_status" %in% cols)) {
    cohort <- cohort |>
    dplyr::left_join(cdm[[n_status_cohort]] |> dplyr::select("subject_id","latest_n_status"), by = "subject_id") |>
    dplyr::mutate(
      latest_n_status = dplyr::coalesce(.data$latest_n_status, "missing")
    ) |>
      dplyr::compute(name = cohort_name)
  }

  if (!("latest_t_status" %in% cols)) {
    cohort <- cohort |>
      dplyr::left_join(cdm[[t_status_cohort]] |> dplyr::select("subject_id","latest_t_status"), by = "subject_id") |>
      dplyr::mutate(
      latest_t_status = dplyr::coalesce(.data$latest_t_status, "missing")
    ) |>
      dplyr::compute(name = cohort_name)
  }
  if (!(all(c("latest_psa_value", "psa_value") %in% cols))) {
    cohort <- cohort |>
      dplyr::select(!dplyr::any_of(c("latest_psa_value", "psa_value"))) |>
    dplyr::left_join(cdm[[psa_cohort]] |> dplyr::select("subject_id","psa_value", "latest_psa_value")) |>

    dplyr::mutate(
      latest_psa_value = dplyr::coalesce(.data$latest_psa_value, "missing")
    ) |>
    dplyr::compute(name = cohort_name)
  }
  return(cohort)
}


getSelectedFeatures <- function(wide_data, cdm, cdm_name) {

  y <- wide_data$y
  df <- wide_data |>
    dplyr::select(
      -dplyr::any_of(c("y", "source", "psa_value")),
      -dplyr::starts_with("latest"),
      -dplyr::ends_with("group")
    )

  mf <- stats::model.frame(
    ~ . - cohort_definition_id - cohort_start_date - cohort_end_date - subject_id + age + year,
    data = df
  )
  # indices of rows that were kept:
  used_rows <- as.integer(rownames(mf))

  X_mat <- stats::model.matrix(
    ~ . - cohort_definition_id - cohort_start_date - cohort_end_date - subject_id + age + year,
    data = df[used_rows,]
  )[, -1, drop = FALSE]
  y_matched <- y[used_rows]   # align y to the rows that model.matrix used

  set.seed(2025)
  lasso_fit <- glmnet::cv.glmnet(x = X_mat, y = y_matched, family = "binomial", alpha = 1)

  coefs <- glmnet::coef.glmnet(lasso_fit, s = "lambda.1se")
  selectedLassoFeatures <- names(coefs[(coefs[,1]!=0),1])
  selectedLassoFeatures <- selectedLassoFeatures[selectedLassoFeatures!="(Intercept)"]
  propensity_scores <- stats::predict(lasso_fit, newx = X_mat, s = "lambda.1se", type = "response")[, 1]
  plot_data <- dplyr::tibble(
    propensity_score = propensity_scores,
    treatment = base::as.factor(y_matched)
  )

  density_points <- export_ps_density(plot_data)

  concepts <- selectedLassoFeatures[!(selectedLassoFeatures %in% c("age", "year"))]
  concepts <-as.integer(as.numeric(stringr::str_extract(concepts, "[0-9][^_]+")))

  tab <- tibble::enframe(coefs[selectedLassoFeatures, ],
                         name = "name",
                         value = "coefficient") |>
    dplyr::mutate(
      name = stringr::str_remove_all(.data$name , "`"),
      variable = dplyr::if_else(name %in% c("age", "year"), .data$name, stringr::str_extract(.data$name, "[0-9][^_]+")),
      window = stringr::str_extract(.data$name, "(?<=_)\\S+$"),
      window = dplyr::coalesce(.data$window, as.character(0L)),
      event = "RP"
    ) |>
    dplyr::select(!"name")

  x <-  cdm$concept |>
    dplyr::filter(.data$concept_id %in% concepts) |>
    dplyr::transmute(
      variable     = as.character(.data$concept_id),
      concept_name = .data$concept_name,
      domain_id    = .data$domain_id
    ) |>
    dplyr::collect() |>
    dplyr::right_join(tab, by = "variable") |>
    dplyr::arrange(coefficient)



  return(list(selected_columns = gsub("^`|`$", "", selectedLassoFeatures),selected_features = x, density_points = density_points))
}

export_ps_density <- function(plot_data, bw = "nrd0", n = 256, kernel = "gaussian") {

  rng <- range(plot_data$propensity_score, na.rm = TRUE)  # common x-range

  dens_df <- lapply(split(plot_data, plot_data$treatment), function(df) {

    d <- stats::density(
      df$propensity_score,
      from = rng[1], to = rng[2],
      bw = bw, n = n, kernel = kernel, na.rm = TRUE
    )
    data.frame(
      treatment = as.character(df$treatment[1]),
      x = d$x,
      y = d$y
    )
  }) |> dplyr::bind_rows()

  return(dens_df)

}

computeASMD <- function(wide_data, features = c()) {

  df <- wide_data |>
    dplyr::select(dplyr::all_of(c("treatment" ="y", features)))|>
    dplyr::mutate(
      treatment = dplyr::recode(as.character(.data$treatment),
                                "0" = "EBRT",
                                "1" = "RP"),
      treatment = factor(.data$treatment, levels = c("EBRT","RP"))
    )

  treatment <- df$treatment

  result <- purrr::map_dfr(features, function(v) {
    x <- df[[v]]
    x0 <- x[df$treatment == "EBRT"]
    x1 <- x[df$treatment == "RP"]



    m0 <- mean(x0, na.rm = TRUE)
    m1 <- mean(x1, na.rm = TRUE)
    v0 <- stats::var(x0, na.rm = TRUE)
    v1 <- stats::var(x1, na.rm = TRUE)

    sd_pooled <- sqrt((v0 + v1) / 2)

    smd <- if (is.finite(sd_pooled) && sd_pooled > 0) {
      (m1 - m0) / sd_pooled
    } else if (isTRUE(all.equal(m1, m0))) {
      0
    } else {
      NA_real_
    }

    tibble::tibble(
      covariate     = v,
      smd          = smd,
      asmd         = abs(smd),
      event        = "RP",
      comparator   = "EBRT"
    )
  }) |> dplyr::bind_rows()

  return(result)
}



### matching ----
getMatchedData <- function(selectedFeatures, wide_data, cdm_name) {

  library(MatchIt)
  selectedFeatures <- c("age", selectedFeatures) |> unique()
  needs_ticks <- selectedFeatures != make.names(selectedFeatures)
  rhs <- ifelse(needs_ticks, sprintf("`%s`", selectedFeatures), selectedFeatures)
  ps_formula <- as.formula(paste("y ~", paste(rhs, collapse = " + ")))

  exact_vars <- c(
    "latest_n_status",
    "latest_gleason_score_value",
    "latest_psa_value",
    "year_group",
    "latest_t_status",
    "source"
  )
  exact_vars <- exact_vars[exact_vars %in% colnames(wide_data)]
  exact_formula <- as.formula(paste("~", paste(exact_vars, collapse = " + ")))
  mf <- stats::model.frame(
    ps_formula,
    data = wide_data
  )
  # indices of rows that were kept:
  used_rows <- as.integer(rownames(mf))
  wide_data <- wide_data[used_rows,]

  set.seed(2025)
  result_matchit <- matchit(data = wide_data,
                            formula = ps_formula,
                            exact = exact_formula,
                            method  = "nearest",
                            ratio   = 1,
                            replace = FALSE,
                            caliper = c(0.2, age = 2),
                            # std.caliper controls whether calipers are interpreted in SD units (TRUE) or raw units (FALSE).
                            # we want the first (PS) in SD units and age caliper in raw years:
                            std.caliper = c(TRUE, FALSE))
  matched_data <- match.data(result_matchit) |>
    dplyr::mutate(pair_id = as.integer(factor(.data$subclass))) |>
    dplyr::relocate(pair_id, .before = dplyr::everything())


  wide_data <- wide_data |>
    dplyr::mutate(
      distance = result_matchit$distance,
      matched = ifelse(.data$subject_id %in% matched_data$subject_id, "Matched", "Unmatched")
    )

  # Combine matched and unmatched
  plot_data <- wide_data |>
    dplyr::filter(!is.na(distance)) |>
    dplyr::mutate(treatment = factor(y))



  return(matched_data)

}

### survival ----

events_summary <- function(survival_data, outcome, covariates = NULL) {


  res <- survival_data |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(
      count_events = sum(status),
      .groups = "drop"
    ) |>
    dplyr::mutate(outcome = outcome)

}


followup_summary <- function(survival_data, outcome, covariates = NULL) {

  overall <- survival_data |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(
      count = dplyr::n(),
      min = min(time),
      q05 = stats::quantile(time, 0.05),
      q25 = stats::quantile(time, 0.25),
      median = stats::median(time),
      q75 = stats::quantile(time, 0.75),
      q95 = stats::quantile(time, 0.95),
      max = max(time),
      .groups = "drop"
    ) |>
    dplyr::mutate(reason = "overall") |>
    dplyr::relocate(treatment, reason)

  by_reason <- survival_data |>
    dplyr::group_by(treatment, reason) |>
    dplyr::summarise(
      count = dplyr::n(),
      min = min(time),
      q05 = stats::quantile(time, 0.05),
      q25 = stats::quantile(time, 0.25),
      median = stats::median(time),
      q75 = stats::quantile(time, 0.75),
      q95 = stats::quantile(time, 0.95),
      max = max(time),
      .groups = "drop"
    )

  dplyr::bind_rows(overall, by_reason) |>
    dplyr::mutate(outcome = outcome)
}

hr_summary <- function(survival_data, outcome, covariates = NULL) {


  adjustment_label <- if (is.null(covariates) || length(covariates) == 0L) "unadjusted" else "adjusted"

  rhs <- c("treatment", covariates)

  formula <- stats::as.formula(
    paste("survival::Surv(time, status) ~", paste(rhs, collapse = " + "))
  )

  fit <- survival::coxph(
    formula = formula,
    data = survival_data,
    na.action = stats::na.exclude,
    ties = "efron",
    cluster = survival_data$pair_id
  )

  coef_tbl <- summary(fit)$coefficients |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select(
      variable,
      coef,
      hr = `exp(coef)`,
      se_coef = `se(coef)`,
      z,
      p = `Pr(>|z|)`
    )

  conf_tbl <- summary(fit)$conf.int |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select(
      variable,
      lower_hr = `lower .95`,
      upper_hr = `upper .95`
    )

  coef_tbl |>
    dplyr::left_join(conf_tbl, by = "variable") |>
    dplyr::mutate(
      adjustment = adjustment_label,
      covariates_used = paste(covariates, collapse = "&"),
      outcome = outcome
    )
}

survival_summary <- function(
    survival_data,
    outcome,
    covariates = NULL,
    risk_times = NULL
) {

  # Default risk_times if user did not supply them (keeps original intention but
  # *does not* forcibly overwrite user-supplied risk_times).
  if (is.null(risk_times)) {
    risk_times <- c(0:max(survival_data$time, na.rm = TRUE))
  }

  # Build group the same way your original did
  group_cols <- c("treatment", covariates)
  if (length(group_cols) == 1L) {
    df <- survival_data |> dplyr::mutate(group = as.factor(.data[[group_cols]]))
  } else {
    df <- survival_data |>
      dplyr::mutate(
        group = interaction(dplyr::across(dplyr::all_of(group_cols)),
                            sep = "&", drop = TRUE)
      )
  }

  df <- df |> dplyr::select("time", "status", "group", "pair_id") |> dplyr::mutate(group = droplevels(group))

  fit <- survival::survfit(survival::Surv(time, status) ~ group, data = df, cluster = pair_id)

  # Request summary at requested times (same call as original)
  s <- summary(fit, times = risk_times)

  # Build treatment exactly like original did; handle NULL strata robustly but keep same parsing
  strata_vec <- if (!is.null(s$strata)) s$strata else rep(NA_character_, length(s$time))

  treatment_vec <- if (all(is.na(strata_vec))) {
    # No strata returned: create a single "all" label (original assumed strata exists;
    # this keeps behaviour explicit and reproducible)
    rep("all", length(s$time))
  } else {
    # remove leading "group=" if present, then take left of "&"
    sub("&.*$", "", stringr::str_replace(strata_vec, "^group=", ""))
  }

  surv_tbl <- tibble::tibble(
    time = s$time,
    survival = s$surv,
    lower_survival = s$lower,
    upper_survival = s$upper,
    treatment = treatment_vec
  ) |>
    dplyr::mutate(
      outcome = outcome,
      adjustment = if (is.null(covariates) || length(covariates) == 0L) "unadjusted" else "adjusted",
      covariates_used = paste(covariates, collapse = "&")
    )

  # Compute number_subjects at each risk time per treatment exactly like original
  risk_df <- purrr::map_df(risk_times, \(rt) {
    survival_data |>
      dplyr::filter(rt <= as.integer(.data$time)) |>
      dplyr::group_by(treatment) |>
      dplyr::summarise(
        count_subjects = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(time = rt)
  })

  surv_tbl |>
    dplyr::left_join(risk_df, by = c("time", "treatment"))
}


outcomeModel <- function(survival_data, outcome, covariates = NULL, risk_times = NULL) {
  include_col <- paste0(outcome, "_include")
  x <- survival_data |>
    dplyr::filter(.data[[include_col]]) |>
    dplyr::rename("outcome" = dplyr::all_of(outcome)) |>
    dplyr::mutate(
      status = dplyr::if_else(is.na(.data$outcome) | .data$outcome > .data$censor_time, 0L, 1L),
      time = dplyr::if_else(.data$status == 0L, .data$censor_time, .data$outcome),
      reason = dplyr::if_else(.data$status == 0L, .data$censor_reason, "outcome"),
      pair_id = as.factor(pair_id)
    ) |>
    dplyr::select("treatment", "status","pair_id", "time","reason", dplyr::any_of(covariates))

  list(
    events_summary   = events_summary(x, outcome, covariates),
    followup_summary = followup_summary(x, outcome, covariates),
    hr_summary       = hr_summary(x, outcome, covariates),
    survival_summary = survival_summary(
      survival_data = x, outcome = outcome, covariates = covariates, risk_times = risk_times
    )
  )
}

NCOModel <- function(survival_data, outcome) {

  x <- survival_data |>
    dplyr::rename(outcome = dplyr::all_of(outcome)) |>
    dplyr::mutate(
      status = dplyr::if_else(is.na(.data$outcome) | .data$outcome >.data$censor_time, 0L, 1L),
      time = dplyr::if_else(.data$status == 0L, .data$censor_time, .data$outcome),
      reason = dplyr::if_else(.data$status == 0L, .data$censor_reason, "outcome"),
      pair_id = as.factor(pair_id)
    ) |>
    dplyr::select("treatment", "status","pair_id", "time","reason", dplyr::any_of(covariates))

  list(
    events_summary   = events_summary(x, outcome),
    hr_summary       = hr_summary(x, outcome, covariates)
  )
}

clean_names <- function(x) {
  x <- base::iconv(x, to = "ASCII//TRANSLIT")
  x <- base::gsub("\\s+", "_", x)            # spaces/tabs -> _
  x <- base::gsub("[^A-Za-z0-9_]", "_", x)   # other punct -> _
  x <- base::gsub("^_+|_+$", "", x)          # trim leading/trailing _
  x <- base::tolower(x)
  x <- base::sub("^([0-9])", "x_\\1", x)     # cannot start with digit
  base::make.unique(x, sep = "_")            # ensure uniqueness
}

bindResults <- function(result, cdmName, cohort_name) {

  variable <- list(
    "hr_summary" = "variable",
    "survival_summary" = "treatment",
    "events_summary" = "treatment",
    "followup_summary" = "treatment"

  )
  strata <- list(
    "hr_summary" = "outcome",
    "survival_summary" = "outcome",
    "events_summary" = "outcome",
    "followup_summary" = "outcome"

  )
  additional <- list(
    "hr_summary" = "covariates_used",
    "survival_summary" = c("time", "covariates_used"),
    "events_summary" = character(0),
    "followup_summary" = "reason"
  )
  estimates <- list(
    "hr_summary" = c("coef", "hr", "se_coef", "z", "p", "lower_hr", "upper_hr"),
    "survival_summary" = c("survival", "lower_survival", "upper_survival", "count_subjects"),
    "events_summary" = "count_events",
    "followup_summary" = c("count", "min", "q05", "q25", "median", "q75", "q95", "max")
  )
  names(result[[1]]) |>
    purrr::map(\(nm) {
      x <- result |>
        purrr::map(\(x) x[[nm]]) |>
        dplyr::bind_rows() |>
        dplyr::select(where(~ !all(is.na(.x) | .x == "")))
      x |>
        dplyr::mutate(
          result_type = nm,
          cdm_name = cdmName,
          cohort = cohort_name
        ) |>
        tidyr::pivot_longer(
          cols = variable[[nm]],
          names_to = "variable_name",
          values_to = "variable_level"
        ) |>
        omopgenerics::transformToSummarisedResult(
          group = "cohort", strata = strata[[nm]], additional = c(character(0), intersect(colnames(x),additional[[nm]])),
          estimates = estimates[[nm]], settings = "result_type"
        )
    }) |>
    omopgenerics::bind()
}



cohortCharacterisation <- function(cdm, cohort_name) {
  len <- cdm[[cohort_name]] |> dplyr::tally()|> dplyr::pull()
  if(len==0){
    return(omopgenerics::emptySummarisedResult())
  }
  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    CohortConstructor::renameCohort(cohortId = 1, newCohortName = paste0("rt_", cohort_name)) |>
    CohortConstructor::renameCohort(cohortId = 2, newCohortName = paste0("rp_", cohort_name)) |>
    addVariables()

  count <- CohortCharacteristics::summariseCohortCount(cdm[[cohort_name]])

  characteristics <- CohortCharacteristics::summariseCharacteristics(cdm[[cohort_name]], cohortIntersectFlag = list(
    "Conditions any time prior" = list(
      targetCohortTable = "conditions", window = c(-Inf, -1)

    ),

    "Medications in the prior year" = list(
      targetCohortTable = "medications", window = c(-365, -1)
    )
  ),
  conceptIntersectCount = list(
    "Inpatient visits prior year" = list(
      conceptSet = list(inpatient_visit = 9201),
      window = c(-365, -1)
    ),
    "Office visits prior year" = list(
      conceptSet = list(office_visit = 581477),
      window = c(-365, -1)
    ),
    "Oncology clinic visits prior year" = list(
      conceptSet = list(oncology_visit = 38004268),
      window = c(-365, -1)
    ),
    "Radiation clinic visits prior year" = list(
      conceptSet = list(radiation_visit = 38004269),
      window = c(-365, -1)
    )
  ),
  otherVariables = c("latest_gleason_score_value", "latest_n_status", "latest_t_status", "psa_value", "latest_psa_value")
  )

  lsc <- CohortCharacteristics::summariseLargeScaleCharacteristics(cdm[[cohort_name]],
                                                                   eventInWindow = c("condition_occurrence", "observation", "procedure_occurrence", "device_exposure"),
                                                                   episodeInWindow = "drug_exposure",
                                                                   window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365), c(366, Inf)),
                                                                   minimumFrequency = 0.0
  )
  result <- omopgenerics::bind(count, characteristics, lsc)

  return(result)

}


mergedCohortCharacterisation <- function(cdm_g, cdm_a, cohort_name) {
  res <- cohortCharacterisation(cdm = cdm_g, cohort_name = cohort_name) |>
    omopgenerics::bind(cohortCharacterisation(cdm = cdm_a, cohort_name = cohort_name))
  set <- omopgenerics::settings(res)
  result_count <- res |>
    dplyr::filter(.data$estimate_name == "count") |>
    dplyr::mutate(cdm_name = "merged") |>
    dplyr::group_by(dplyr::across(-c("estimate_value"))) |>
    dplyr::summarise(estimate_value = as.character(sum(as.numeric(.data$estimate_value))), .groups = "drop")


  den <- result_count |>
    dplyr::filter(.data$variable_name == "Number subjects") |>
    dplyr::select(!"result_id") |>
    dplyr::distinct() |>
    dplyr::mutate("den" = as.numeric(.data$estimate_value)) |>
    dplyr::select("group_level", "den")

  result_perc <-  result_count |>
    dplyr::filter(!(.data$variable_name %in% c("Number subjects", "Number records"))) |>
    dplyr::left_join(den) |>
    dplyr::group_by(dplyr::across(- "estimate_value")) |>
    dplyr::summarise(estimate_value = as.numeric(.data$estimate_value)/den *100, .groups = "drop") |>
    dplyr::mutate(estimate_name = "percentage",
                  estimate_type = "percentage",
                  estimate_value = sprintf("%.3f", .data$estimate_value)) |>
    dplyr::select(-"den")

  g <- cdm_g[[cohort_name]] |>
    PatientProfiles::addFutureObservation() |>
    PatientProfiles::addPriorObservation() |>
    PatientProfiles::addAge() |>
    PatientProfiles::addTableIntersectCount(tableName = "visit_occurrence", window = c(-365, -1), nameStyle = "number_visits_prior_year") |>
    PatientProfiles::addCohortName() |>
    dplyr::mutate(source = "gold") |>
    dplyr::collect()
  a <- cdm_a[[cohort_name]] |>
    PatientProfiles::addFutureObservation() |>
    PatientProfiles::addPriorObservation() |>
    PatientProfiles::addAge() |>
    PatientProfiles::addTableIntersectCount(tableName = "visit_occurrence", window = c(-365, -1),  nameStyle = "number_visits_prior_year") |>
    PatientProfiles::addCohortName() |>
    dplyr::mutate(source = "aurum") |>
    dplyr::collect()
  result_cont <- g |>
    dplyr::bind_rows(
      a) |>
    PatientProfiles::summariseResult(group = "cohort_name",variables = c("source", "age", "future_observation", "prior_observation", "psa_value", "cohort_start_date", "cohort_end_date", "number_visits_prior_year"),
                                     estimates = c("min", "q25", "median", "q75", "max", "mean", "sd","count", "percentage")) |>
    dplyr::mutate(result_id = 2,
                  cdm_name = "merged",
                  variable_name =  gsub("_", " ", .data$variable_name),
                  variable_name = paste0(toupper(substring(.data$variable_name, 1, 1)),
                                         substring(.data$variable_name, 2)))

  result <- dplyr::bind_rows(result_count, result_perc, result_cont) |>
    dplyr::distinct() |>
    omopgenerics::newSummarisedResult(settings = set)


 return(result)


}

deathSurvival <- function(cdm,
                          cohort_name
                         ) {
  cdm[[cohort_name]] |>
    PatientProfiles::addDeathDays(deathDaysName = "death", window = c(1, Inf)) |>
    PatientProfiles::addFutureObservation(futureObservationName = "end_of_observation") |>
    dplyr::mutate(death = dplyr::coalesce(.data[["death"]], 999999L)) |>
    dplyr::mutate(
      censor_time = dplyr::case_when(
        .data$death <= .data$end_of_observation ~ .data$death,
        .default = .data$end_of_observation
      ),
      censor_reason = dplyr::case_when(
        .data$censor_time == .data$death ~ "death",
        .data$censor_time == .data$end_of_observation ~ "end of observation",
        .default = "error"
      ),
      treatment = dplyr::case_when(
        .data$cohort_definition_id == 1L ~ "EBRT",
        .data$cohort_definition_id == 2L ~ "RP",
        TRUE ~ as.character(.data$cohort_definition_id)
      )
    ) |>
    dplyr::compute(name = cohort_name)
}
addCauseOfDeath <- function(cohort, death_pc_codes, death_cvd_codes) {
  cohort |>
    PatientProfiles::addTableIntersectField(
    tableName = "death",
    order = "first",
    field = "cause_concept_id",
    nameStyle = "cause_of_death") |>
    dplyr::mutate(
      cause_of_death = as.integer(as.numeric(.data$cause_of_death)),
      death_pc  = dplyr::if_else(
        !is.na(.data$cause_of_death) & .data$cause_of_death %in% death_pc_codes,
        .data$death, 999999L
      ),
      death_cvd = dplyr::if_else(
        !is.na(.data$cause_of_death) & .data$cause_of_death %in% death_cvd_codes,
        .data$death, 999999L
      )
    ) |>
    dplyr::select(!"cause_of_death")
}
addOutcome <- function(cohort, outcome, outcome_codelist) {
  washout_name <- paste0(outcome, "_washout")
  post_window <- if (outcome == "androgen_deprivation") list(c(181, Inf)) else list(c(1, Inf))
  pre_window  <- list(c(-365, 0))
  table_name <- omopgenerics::tableName(cohort)
  if (outcome != "type2_diabetes") {
    cohort <- cohort |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = list(outcome = outcome_codelist[[outcome]]),
        window = post_window,
        order = "first",
        nameStyle = outcome,
        name = table_name
      ) |>
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = list(outcome = outcome_codelist[[outcome]]),
        window = pre_window,
        targetStartDate = "event_end_date",
        name = table_name,
        nameStyle = washout_name
      ) |>
      dplyr::mutate(!!rlang::sym(outcome) := dplyr::coalesce(.data[[outcome]], 999999L))
  } else {
    cohort <- cohort |>
      PatientProfiles::addCohortIntersectDays(
        targetCohortTable = "type2_diabetes",
        window = post_window,
        order = "first",
        nameStyle = "type2_diabetes",
        name = table_name
      ) |>
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "type2_diabetes",
        window = pre_window,
        targetStartDate = "cohort_end_date",
        name = table_name,
        nameStyle = washout_name
      ) |>
      dplyr::mutate(!!rlang::sym(outcome) := dplyr::coalesce(.data[[outcome]], 999999L))
  }
  pairs <- cohort |>
    dplyr::filter(.data[[washout_name]] == 1L) |>
    dplyr::pull(.data$pair_id) |>
    unique()
 include_col <- paste0(outcome, "_include")
 cohort |>
   dplyr::mutate(!!rlang::sym(include_col) := dplyr::if_else(.data$pair_id %in% pairs, FALSE, TRUE)) |>
   dplyr::select(!dplyr::all_of(washout_name)) |>
   dplyr::compute(table_name)
}

