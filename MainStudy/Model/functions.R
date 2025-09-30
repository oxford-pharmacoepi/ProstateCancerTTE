### propensity scores ----

longDataFromCohort <- function(cdm, cohort_name = "optima_pc_rwd"){
  cohort_name_long <- paste(cohort_name, "long", sep = "_")
  cdm[[cohort_name_long]] <- cdm[[cohort_name]]|>
    dplyr::left_join(cdm$condition_occurrence |> dplyr::select(person_id, condition_concept_id, condition_start_date), by = c("subject_id" = "person_id")) |>
    dplyr::filter(.data$condition_start_date < .data$cohort_start_date) |>
    dplyr::rename("concept_id" = "condition_concept_id")|>
    dplyr::select(!c(condition_start_date))|>
    dplyr::union_all( cdm[[cohort_name]] |>
                        dplyr::left_join(cdm$drug_exposure |> dplyr::select(person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date), by = c("subject_id" = "person_id")) |>
                        dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$drug_exposure_start_date),
                                      days_diff_end = as.integer(.data$cohort_start_date - .data$drug_exposure_end_date)) |>
                        dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start > 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end > 0 ) | (.data$cohort_start_date > .data$drug_exposure_start_date & .data$cohort_start_date < .data$drug_exposure_end_date)) |>
                        dplyr::rename("concept_id" = "drug_concept_id") |>
                        dplyr::select(!c(drug_exposure_start_date, drug_exposure_end_date, days_diff_start, days_diff_end))) |>
    dplyr::compute(name = cohort_name_long)

  cdm[[cohort_name_long]] <- cdm[[cohort_name_long]] |>
    dplyr::right_join(cdm[[cohort_name]]) |>
    dplyr::compute(name = cohort_name_long)

return(cdm)
}

getFrequentConcepts <- function(cohort, excluded_codes){
  n <- cohort |>
    dplyr::distinct(subject_id) |>
    dplyr::tally() |>
    dplyr::pull()|>
    as.numeric()

  frequent_concepts <- cohort |>
    dplyr::filter(!(.data$concept_id %in% excluded_codes))|>
    dplyr::group_by(.data$concept_id) |>
    dplyr::summarise(n_subjects = (dplyr::n_distinct(.data$subject_id)), .groups = "drop") |>
    dplyr::mutate(perc = .data$n_subjects / .env$n) |>
    dplyr::filter(perc >= 0.005)|> dplyr::pull(concept_id)

return(frequent_concepts)
}

visitsCount <- function(cdm, cohort_name = "optima_pc_rwd"){
  cohort_name_visits <- paste(cohort_name, "visits", sep = "_")
  cdm[[cohort_name_visits]] <- cdm[[cohort_name]] |>
    dplyr::left_join(cdm$visit_occurrence |>
                       dplyr::filter(.data$visit_concept_id != 38004268) |>
                       dplyr::select("person_id", "visit_concept_id", "visit_start_date", "visit_end_date"),
                     by = c("subject_id" = "person_id")) |>
    dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$visit_start_date),
                  days_diff_end = as.integer(.data$cohort_start_date - .data$visit_end_date)) |>
    dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start >= 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end >= 0 ) | (.data$cohort_start_date > .data$visit_start_date & .data$cohort_start_date < .data$visit_end_date)) |>
    dplyr::group_by(.data$subject_id, .data$visit_concept_id, .data$cohort_definition_id, .data$cohort_start_date, .data$cohort_end_date) |>
    dplyr::tally() |>
    tidyr::pivot_wider(names_from = "visit_concept_id", values_from = "n", names_prefix = "visit_", values_fill = list(n = 0)) |>

    dplyr::compute(name = cohort_name_visits)

  cdm[[cohort_name_visits]] <- cdm[[cohort_name_visits]] |>
    dplyr::right_join(cdm[[cohort_name]]) |>
    dplyr::compute(name = cohort_name_visits)
  return(cdm)
}
getWideData <- function(cohort, frequent_concepts, visits) {
  id_vars <- c(
    "cohort_definition_id","subject_id","cohort_start_date","cohort_end_date",
    "latest_gleason_score_value","latest_n_status","latest_psa_value",
    "age","age_group","source"     # drop if not present
  )
  wide_data <-cohort |>
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
    dplyr::mutate(year = clock::get_year(cohort_start_date),
                  visit_9201 = dplyr::coalesce(visit_9201, 0),
                  visit_581477 = dplyr::coalesce(visit_581477, 0)) |>
    dplyr::mutate(year_group = dplyr::case_when(
      year < 2010 ~ "<2010",
      year >= 2010 & year < 2015 ~ "2010-2015",
      year >= 2015 & year < 2020 ~ "2015-2020",
      year >= 2020 ~ ">2020",
      TRUE ~ NA_character_
      ))

  return(wide_data)
}

addVariables <- function(cdm, cohort_name = "optima_pc_rwd" ) {
  cols <- colnames(cdm[[cohort_name]])

  if (!("latest_psa_value" %in% cols)){
    cdm[[cohort_name]] <- cdm[[cohort_name]] |>
      dplyr::mutate(
        psa_value = dplyr::coalesce(.data$psa_value, -1)
      ) |>
      PatientProfiles::addCategories(variable = "psa_value", categories = list("latest_psa_value" = list("<3" = c(0, 2.99),
                                                                                                       "3 to 19.99" = c(3,19.99),
                                                                                                       "20 to 39.99" = c(20, 39.99),
                                                                                                       ">40" = c(40, Inf),
                                                                                                       "missing" = c(-1,-1))
                                                                               ), name = cohort_name) |>

      dplyr::compute(name = cohort_name)
  }
  if (!("latest_gleason_score_value" %in% cols)) {
  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    PatientProfiles::addConceptIntersectField(conceptSet = list("gleason_score" = 619648),
                                              field = "value_as_number",
                                              indexDate = "cohort_start_date",
                                              order = "last",
                                              window = c(-180,0),
                                              name = cohort_name,
                                              allowDuplicates = FALSE,
                                              nameStyle = "gleason") |>
    dplyr::mutate(
      gleason = dplyr::coalesce(.data$gleason, -1)
    ) |>
    PatientProfiles::addCategories(variable = "gleason",
                                   categories = list("latest_gleason_score_value" = list("<2" = c(0,1),
                                                                                         "2 to 6" = c(2,6),
                                                                                         "7" = c(7,7),
                                                                                         "8 to 10" = c(8,10),
                                                                                         ">10" = c(11, Inf),
                                                                                         "missing" = c(-1,-1))
                                   ),
                                   name = cohort_name)
  }
  if (!("latest_n_status" %in% cols)) {
    N_status_codelist <- omopgenerics::importCodelist(here::here("Codelist/Characterisation/N-status"), type = "csv")

    cdm[[cohort_name]] <- cdm[[cohort_name]] |>
      PatientProfiles::addConceptIntersectDate(conceptSet = N_status_codelist,

                                             indexDate = "cohort_start_date",
                                             order = "last",
                                             window = c(-Inf,0),
                                             name = cohort_name,
                                             nameStyle = "{concept_name}"
                                             ) |>
      dplyr::mutate(
        n_date = pmax(n0, nx, n1, n2, n3, na.rm = TRUE),
        latest_n_status = dplyr::case_when(
          is.na(n0) & is.na(nx) & is.na(n1) & is.na(n2) & is.na(n3) ~ "missing",
          n0 == n_date ~ "n0",
          nx == n_date ~ "nx",
          n1 == n_date ~ "n1",
          n2 == n_date ~ "n2",
          n3 == n_date ~ "n3",
          TRUE ~ NA_character_
          )) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::filter(.data$latest_n_status == "missing" | .data$n_date == max(n_date, na.rm = TRUE))  |>
      dplyr::ungroup() %>%
      dplyr::select(-n0, -nx, -n1, -n2, -n3) |>
      dplyr::compute(name = cohort_name)
  }

  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", dplyr::starts_with("latest") ) |>
    dplyr::compute(name = cohort_name)
  return(cdm)
}


getSelectedFeatures <- function(wide_data, directory, cdm, cdm_name) {
  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0),
    )

  y <- wide_data$y
  X <- wide_data |>
    dplyr::select(-c("y",dplyr::starts_with("latest"), dplyr::ends_with("group")))

  X <- stats::model.matrix(
    ~ . - cohort_definition_id - cohort_start_date - cohort_end_date - subject_id + age + year ,
    data = X
  )[, -1]
  set.seed(2025)
  lasso_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    family = "binomial",
    alpha = 1
  )

  coefs <- glmnet::coef.glmnet(lasso_fit, s = "lambda.1se")
  selectedLassoFeatures <- names(coefs[(coefs[,1]!=0),1])
  selectedLassoFeatures <- selectedLassoFeatures[selectedLassoFeatures!="(Intercept)"]
  propensity_scores <- stats::predict(lasso_fit, newx = X, s = "lambda.1se", type = "response")[, 1]
  plot_data <- dplyr::tibble(
    propensity_score = propensity_scores,
    treatment = base::as.factor(y)
  )

  density_points <- export_ps_density(plot_data)

  concepts <- selectedLassoFeatures[!(selectedLassoFeatures %in% c("age", "year"))]
  concepts <-as.integer(as.numeric(gsub("[^0-9]", "", concepts)))


  tab <- tibble::enframe(coefs[selectedLassoFeatures, ],
                         name = "variable",
                         value = "coefficient") |>
    dplyr::mutate(variable = dplyr::if_else(.data$variable %in% c("age", "year"), .data$variable, gsub("[^0-9]", "", .data$variable)),
                  event = "RP")

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
      event    = "RP",
      comparator   = "EBRT"
    )
  }) |> dplyr::bind_rows()

  return(result)
}



### matching ----
getMatchedData <- function(selectedFeatures, wide_data, directory, cdm_name) {

  library(MatchIt)

  needs_ticks <- selectedFeatures != make.names(selectedFeatures)
  rhs <- ifelse(needs_ticks, sprintf("`%s`", selectedFeatures), selectedFeatures)
  ps_formula <- as.formula(paste("y ~", paste(rhs, collapse = " + ")))

  exact_vars <- c(
    "latest_n_status",
    "latest_gleason_score_value",
    "latest_psa_value",
    "year_group",
    "age_group",
    "source"
  )
  exact_vars <- exact_vars[exact_vars %in% colnames(wide_data)]
  exact_formula <- as.formula(paste("~", paste(exact_vars, collapse = " + ")))

  set.seed(2025)
  result_matchit <- matchit(data = wide_data,
                            formula = ps_formula,
                            exact = exact_formula,
                            method  = "nearest",
                            ratio   = 1,
                            replace = FALSE,
                            caliper = 0.2)
  matched_data <- match.data(result_matchit) |>
    dplyr::mutate(pair_id = as.integer(factor(.data$subclass))) |>
    dplyr::relocate(pair_id, .before = dplyr::everything())

  library(ggplot2)
  # p <- ggplot(matched_data, aes(x = factor(y), y = distance)) +
  #   geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  #   labs(
  #     x = "Treatment Group",
  #     y = "Propensity Score",
  #     title = "Propensity Score Distribution (Matched Sample)"
  #   ) +
  #   theme_minimal()
  #
  # # Save plot
  # ggsave(
  #   filename = file.path(directory, paste0(cdm_name, "_ps_matched.png")),
  #   plot = p,
  #   width = 7,
  #   height = 5,
  #   units = "in",
  #   dpi = 300
  # )

  wide_data <- wide_data |>
    dplyr::mutate(
      distance = result_matchit$distance,
      matched = ifelse(.data$subject_id %in% matched_data$subject_id, "Matched", "Unmatched")
    )

  # Combine matched and unmatched
  plot_data <- wide_data |>
    dplyr::filter(!is.na(distance)) |>
    dplyr::mutate(treatment = factor(y))

  # Plot histogram of propensity scores by treatment group and match status
  # p <- ggplot(plot_data, aes(x = distance, fill = treatment)) +
  #   geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  #   facet_wrap(~ matched) +
  #   labs(
  #     x = "Propensity Score",
  #     y = "Count",
  #     fill = "Treatment",
  #     title = "Propensity Score Distribution by Matching Status"
  #   ) +
  #   theme_minimal()
  #
  # ggsave(
  #   filename = file.path(directory, paste0(cdm_name, "_ps_matched_vs_unmatched.png")),
  #   plot = p,
  #   width = 7,
  #   height = 5,
  #   units = "in",
  #   dpi = 300
  # )


  return(matched_data)

}

### survival ----

outcomeModel <- function(survival_data, outcome, covariates = NULL, risk_times = c(0, 365, 2*365, 3*365, 4*365, 5*365, 10*365)) {

  x <- survival_data |>
    dplyr::rename(outcome = dplyr::all_of(outcome)) |>
    dplyr::mutate(
      status = dplyr::if_else(is.na(.data$outcome) | .data$outcome >.data$censor_time, 0L, 1L),
      time = dplyr::if_else(.data$status == 0L, .data$censor_time, .data$outcome),
      reason = dplyr::if_else(.data$status == 0L, .data$censor_reason, "outcome"),
      pair_id = as.factor(pair_id)
    ) |>
    dplyr::select("treatment", "status","pair_id", "time","reason", dplyr::any_of(covariates))



  events <- x |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(
      number_events = sum(status),
     .groups = "drop"
    ) |>
    dplyr::mutate(outcome = outcome)

  followup_summary <- x |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(
      n = dplyr::n(),
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
    dplyr::relocate("treatment", "reason") |>
    dplyr::union_all(
      x |>
        dplyr::group_by(treatment, reason) |>
        dplyr::summarise(
          n = dplyr::n(),

          min = min(time),
          q05 = stats::quantile(time, 0.05),
          q25 = stats::quantile(time, 0.25),
          median = stats::median(time),
          q75 = stats::quantile(time, 0.75),
          q95 = stats::quantile(time, 0.95),
          max = max(time),
          .groups = "drop"
        )
    ) |>
    dplyr::mutate(outcome = outcome)


  adjustment_label <- if (is.null(covariates) || length(covariates) == 0L) "unadjusted" else "adjusted"

  rhs <- c("treatment", covariates)
  formula <- stats::as.formula(
    paste("survival::Surv(time, status) ~", paste(rhs, collapse = " + "))
  )
  cox_fit <- survival::coxph(formula = survival::Surv(time, status) ~ treatment, data = x, na.action = stats::na.exclude, ties = "efron", cluster = x$pair_id)


  result <- summary(cox_fit)$coefficients |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select(
      "variable", "coef", "hr" = "exp(coef)", "se_coef" = "se(coef)", "z",
      "p" = "Pr(>|z|)"
    ) |>
    dplyr::left_join(
      summary(cox_fit)$conf.int %>%
        tibble::as_tibble(rownames = "variable") %>%
        dplyr::select("variable", "lower_hr" = "lower .95", "upper_hr" = "upper .95"),
      by = "variable"
    ) |>
    dplyr::mutate(
      adjustment = adjustment_label,
      covariates_used = paste(covariates, collapse = "&"),
      outcome = outcome
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ sprintf("%.3f", .x))
    )

  group_cols = c("treatment", covariates)


  if (length(group_cols) == 1L) {
    df <- x |> dplyr::mutate(group = as.factor(.data[[group_cols]]))
  } else {
    df <- x |>
      dplyr::mutate(
        group = interaction(dplyr::across(dplyr::all_of(group_cols)),
                            sep = "&", drop = TRUE)
      )
  }

  df <- df |> dplyr::select("time", "status", "group", "pair_id") |> dplyr::mutate(group = droplevels(group))
  fit <- survival::survfit(survival::Surv(time, status) ~ group, data = df, cluster = pair_id)

  risk_times <- c(0:max(survival_data$censor_time))
  survival_summary <- summary(fit, times = c(0:max(survival_data$censor_time)))
  survival_summary <- tibble::tibble(
    time = survival_summary$time,
    survival = survival_summary$surv,
    lower_survival = survival_summary$lower,
    upper_survival = survival_summary$upper,
    treatment = sub("&.*$", "", stringr::str_replace(survival_summary$strata, "^group=", ""))
  ) |>
    dplyr::mutate(outcome = outcome,
                  adjustment = adjustment_label,
                  covariates_used = paste(covariates, collapse = "&")) |>
    dplyr::left_join(
      risk_times |>
        purrr::map_df(\(rt) {

          x |>
            dplyr::filter(rt <= as.integer(.data$time)) |>
            dplyr::group_by(treatment) |>
            dplyr::summarise(
              number_subjects = dplyr::n(),

              .groups = "drop"
            ) |>
            dplyr::mutate(time = rt)
        }),
      by = c("time", "treatment")
    )

  list(
    events_summary = events,
    followup_summary = followup_summary,
    survival_summary = survival_summary,
    hr_summary = result
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
    "survival_summary" = c("survival", "lower_survival", "upper_survival", "number_subjects"),
    "events_summary" = "number_events",
    "followup_summary" = c("n", "min", "q05", "q25", "median", "q75", "q95", "max")
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
