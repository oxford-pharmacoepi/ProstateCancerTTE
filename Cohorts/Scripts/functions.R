addCharacteristics <- function(cohort) {
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


  if (!("age_group" %in% cols)){
    cohort <- cohort |>
      PatientProfiles::addDemographics(age = FALSE,
                                       sex = FALSE,
                                       priorObservation = FALSE,
                                       futureObservation = FALSE,
                                       ageGroup = list(c(50, 69), c(70, Inf)),
                                       name = cohort_name)
  }

  if (!("year_group"%in% cols)){
    cohort <- cohort |>
      dplyr::mutate(year_2010_2020 = dplyr::if_else(.data$cohort_start_date <= as.Date("2019-12-31")
                                                    && (.data$cohort_start_date >= as.Date("2010-01-01")),
                                                    "Yes",
                                                    "No" )) |>
      dplyr::compute(name = cohort_name)
  }
  return(cohort)
}

