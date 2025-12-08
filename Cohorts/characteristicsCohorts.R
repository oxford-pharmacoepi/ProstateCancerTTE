# characteristics ----


## N status ----
N_status_codelist <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/Characterisation/N-status", type = "csv")

cdm[["n_status"]] <- CohortConstructor::conceptCohort(cdm, conceptSet = N_status_codelist,
                                                      subsetCohort = "optima_pc_rwd",
                                                      name = "n_status")|>
  PatientProfiles::addCohortName() |>
  dplyr::rename("latest_n_status" = "cohort_name")


cdm[["n_status_rwd"]] <- cdm[["n_status"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_rwd", window = c(0,  Inf), name = "n_status_rwd") |>
  dplyr::group_by(.data$subject_id) |>
  dplyr::slice_max(.data$cohort_start_date) |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  dplyr::compute(name = "n_status_rwd")

cdm[["n_status_trial"]] <- cdm[["n_status"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_trial", window = c(0,  180),  name = "n_status_trial") |>
  dplyr::group_by(.data$subject_id) |>
  dplyr::slice_max(.data$cohort_start_date) |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  dplyr::compute(name = "n_status_trial")

## T status ----

t1_status <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/Characterisation/conditions/t1.csv", type = "csv")
t2_status <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/Characterisation/conditions/t2.csv", type = "csv")
t_status_codelist <- omopgenerics::bind(t1_status, t2_status)

cdm[["t_status"]] <- CohortConstructor::conceptCohort(cdm, conceptSet = t_status_codelist,
                                                      subsetCohort = "optima_pc_rwd",
                                                      name = "t_status")|>
  PatientProfiles::addCohortName() |>
  dplyr::rename("latest_t_status" = "cohort_name")

cdm[["t_status_rwd"]] <- cdm[["t_status"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_rwd", window = c(0,  Inf), name = "t_status_rwd") |>
  dplyr::group_by(.data$subject_id) |>
  dplyr::slice_max(.data$cohort_start_date) |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  dplyr::compute(name = "t_status_rwd")

cdm[["t_status_trial"]] <- cdm[["t_status"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_trial", window = c(0,  180), name = "t_status_trial") |>
  dplyr::group_by(.data$subject_id) |>
  dplyr::slice_max(.data$cohort_start_date) |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  dplyr::compute(name = "t_status_trial")

## Gleason score ----

cdm[["gleason"]] <- cdm$measurement |>
  dplyr::filter(.data$measurement_concept_id %in% 619648) |>
  dplyr::select("person_id" , "measurement_date", "value_as_number") |>
  dplyr::group_by(.data$person_id, .data$measurement_date) |>
  dplyr::summarise(
    min_val         = min(.data$value_as_number, na.rm = TRUE),
    max_val         = max(.data$value_as_number, na.rm = TRUE),
    all_na          = dplyr::if_else(sum(as.integer(!is.na(.data$value_as_number))) == 0L,TRUE, FALSE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    value_as_number = dplyr::case_when(
      all_na ~ NA_real_,
      min_val == max_val ~ min_val,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(.data$value_as_number)) |>
  dplyr::select("subject_id" = "person_id", "cohort_start_date" = "measurement_date", "cohort_end_date" = "measurement_date",
                "gleason" = "value_as_number"
  )|>
  dplyr::mutate("cohort_definition_id" = 1L) |>
  PatientProfiles::filterInObservation(indexDate = "cohort_start_date") |>
  dplyr::compute(name = "gleason") |>
  omopgenerics::newCohortTable()



cdm[["gleason_rwd"]] <- cdm[["gleason"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_rwd", window = c(0,  Inf), name = "gleason_rwd") |>
  CohortConstructor::requireIsLastEntry() |>
  PatientProfiles::addCategories(variable = "gleason",
                                 categories = list("latest_gleason_score_value" = list("<2" = c(0,1),
                                                                                       "2 to 6" = c(2,6),
                                                                                       "7" = c(7,7),
                                                                                       "8 to 10" = c(8,10),
                                                                                       ">10" = c(11, Inf)
                                 )
                                 ),
                                 name = "gleason_rwd"
  )



cdm[["gleason_trial"]] <- cdm[["gleason"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_trial", window = c(0,  180), name = "gleason_trial") |>
  CohortConstructor::requireIsLastEntry() |>
  PatientProfiles::addCategories(variable = "gleason",
                                 categories = list("latest_gleason_score_value" = list("<2" = c(0,1),
                                                                                       "2 to 6" = c(2,6),
                                                                                       "7" = c(7,7),
                                                                                       "8 to 10" = c(8,10),
                                                                                       ">10" = c(11, Inf)
                                 )
                                 ),
                                 name = "gleason_trial"
  )




