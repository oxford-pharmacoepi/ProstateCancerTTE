longDataFromCohort <- function(cdm){

  cdm$optima_pc_rwd_long <- cdm$optima_pc_rwd |>
    dplyr::left_join(cdm$condition_occurrence |> dplyr::select(person_id, condition_concept_id, condition_start_date), by = c("subject_id" = "person_id")) |>
    dplyr::filter(.data$condition_start_date < .data$cohort_start_date) |>
    dplyr::rename("concept_id" = "condition_concept_id")|>
    dplyr::select(!c(condition_start_date))|>
    dplyr::union_all( cdm$optima_pc_rwd |>
                        dplyr::left_join(cdm$drug_exposure |> dplyr::select(person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date), by = c("subject_id" = "person_id")) |>
                        dplyr::mutate(days_diff_start = as.integer(.data$cohort_start_date - .data$drug_exposure_start_date),
                                      days_diff_end = as.integer(.data$cohort_start_date - .data$drug_exposure_end_date)) |>
                        dplyr::filter((.data$days_diff_start <= 365 & .data$days_diff_start > 0 ) | (.data$days_diff_end <= 365 & .data$days_diff_end > 0 ) | (.data$cohort_start_date > .data$drug_exposure_start_date & .data$cohort_start_date < .data$drug_exposure_end_date)) |>
                        dplyr::rename("concept_id" = "drug_concept_id") |>
                        dplyr::select(!c(drug_exposure_start_date, drug_exposure_end_date, days_diff_start, days_diff_end))) |>
    dplyr::compute(name = "optima_pc_rwd_long")

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

visitsCount <- function(cdm){
  cdm$optima_pc_rwd_visits <- cdm$optima_pc_rwd |>
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

    dplyr::compute(name = "optima_pc_rwd_visits")
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
    dplyr::left_join(visits,
                     by = c("subject_id", "cohort_definition_id", "cohort_start_date", "cohort_end_date")) |>
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

  wide_data <- wide_data |>
    dplyr::distinct() |>
    dplyr::mutate(y = ifelse(cohort_definition_id == 2, 1, 0),
    )
  return(wide_data)
}

addVariables <- function(cdm) {
  cols <- colnames(cdm$optima_pc_rwd)

  if (!("latest_psa_value" %in% cols)){
    cdm$optima_pc_rwd |>
    PatientProfiles::addCategories(variable = "psa_value", categories = list("latest_psa_value" = list("<3" = c(-Inf, 2.99),
                                                                                                       "3 to 19.99" = c(3,19.99),
                                                                                                       "20 to 39.99" = c(20, 39.99),
                                                                                                       ">40" = c(40, Inf))
    ), name = "optima_pc_rwd") |>
    dplyr::mutate(latest_psa_value = dplyr::coalesce(.data$latest_psa_value, "none"))
  }
  if (!("latest_gleason_score_value" %in% cols)) {
  cdm$optima_pc_rwd <- cdm$optima_pc_rwd |>
    PatientProfiles::addConceptIntersectField(conceptSet = list("gleason_score" = 619648),
                                              field = "value_as_number",
                                              indexDate = "cohort_start_date",
                                              order = "last",
                                              window = c(-180,0),
                                              name = cohort_name,
                                              allowDuplicates = FALSE,
                                              nameStyle = "gleason") |>
    PatientProfiles::addCategories(variable = "gleason",
                                   categories = list("latest_gleason_score_value" = list("<2" = c(0,1),
                                                                                         "2 to 6" = c(2,6),
                                                                                         "7" = c(7,7),
                                                                                         "8 to 10" = c(8,10),
                                                                                         ">10" = c(11, Inf))
                                   ),
                                   name = "optima_pc_rwd")
  }
  if (!("latest_n_status" %in% cols)) {
    N_status_codelist <- omopgenerics::importCodelist(here::here("Codelist/Characterisation/N-status"), type = "csv")

  cdm$optima_pc_rwd <- cdm$optima_pc_rwd |>
    PatientProfiles::addConceptIntersectDate(conceptSet = N_status_codelist,

                                             indexDate = "cohort_start_date",
                                             order = "last",
                                             window = c(-Inf,0),
                                             name = "optima_pc_rwd",
                                             nameStyle = "{concept_name}"
    ) |>
    dplyr::mutate(
      n_date = pmax(n0, nx, n1, n2, n3, na.rm = TRUE),
      latest_n_status = dplyr::case_when(
        n0 == n_date ~ "n0",
        nx == n_date ~ "nx",
        n1 == n_date ~ "n1",
        n2 == n_date ~ "n2",
        n3 == n_date ~ "n3",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::group_by(subject_id) %>%
    dplyr::filter(n_date == max(n_date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n0, -nx, -n1, -n2, -n3) |> dplyr::compute(name = "optima_pc_rwd")
  }

  cdm$optima_pc_rwd <- cdm$optima_pc_rwd |>
    dplyr::mutate(latest_psa_value = dplyr::coalesce(.data$latest_psa_value, "none"),
                  latest_n_status = dplyr::coalesce(.data$latest_n_status, "none"),
                  latest_gleason_score_value = dplyr::coalesce(.data$latest_gleason_score_value, "none")) |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", dplyr::starts_with("latest") ) |>
    dplyr::compute(name = "optima_pc_rwd")
  return(cdm)
}


getSelectedFeatures <- function(wide_data, directory, cdm) {
  y <- wide_data$y
  X <- wide_data |>
    dplyr::select(-c("y",dplyr::starts_with("latest"), dplyr::ends_with("group")))
  X <- stats::model.matrix(
    ~ . - cohort_definition_id - cohort_start_date - cohort_end_date - subject_id + age + year ,
    data = X
  )[, -1]
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
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = propensity_score, fill = treatment)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(
      title = "Distribution of propensity scores by treatment",
      x = "Propensity score",
      fill = "Treatment"
    ) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(
    filename = paste0(directory, "/propensity_plot.png"),
    plot = p,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )

  concepts <- selectedLassoFeatures[!(selectedLassoFeatures %in% c("age", "year"))]
  coefs_concepts <- coefs[concepts,]
  concepts <- gsub("[^0-9]", "", concepts)

  names(coefs_concepts) <- concepts

  x <- tibble::tibble("concept_id" = as.integer(names(coefs_concepts)), "coefficient" = unname(coefs_concepts))

  res <- cdm$concept |>
    dplyr::filter(.data$concept_id %in% concepts) |>
    dplyr::select(concept_id, concept_name, domain_id) |>
    dplyr::collect()|>
    dplyr::left_join(x, by = "concept_id") |>
    dplyr::arrange(coefficient)

  write.csv(res, file = paste0(directory, "/lasso_coefs.csv"), row.names = FALSE)

  return(gsub("`", "", selectedLassoFeatures))
}

getMatchedData <- function(selectedFeatures, wide_data, directory) {

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


  result_matchit <- matchit(data = wide_data,
                            formula = ps_formula,
                            exact = exact_formula,
                            caliper = 0.2)
  matched_data <- match.data(result_matchit)

  library(ggplot2)
  p <- ggplot(matched_data, aes(x = factor(y), y = distance)) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
    labs(
      x = "Treatment Group",
      y = "Propensity Score",
      title = "Propensity Score Distribution (Matched Sample)"
    ) +
    theme_minimal()

  # Save plot
  ggsave(
    filename = paste0(directory, "/ps_matched.png"),
    plot = p,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )

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
  p <- ggplot(plot_data, aes(x = distance, fill = treatment)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    facet_wrap(~ matched) +
    labs(
      x = "Propensity Score",
      y = "Count",
      fill = "Treatment",
      title = "Propensity Score Distribution by Matching Status"
    ) +
    theme_minimal()

  ggsave(
    filename = paste0(directory, "/ps_matched_vs_unmatched.png"),
    plot = p,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )


  return(result_matchit)

}
