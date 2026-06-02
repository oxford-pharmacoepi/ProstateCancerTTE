# characteristics ----

omopgenerics::logMessage("Start: build characteristic cohorts (N and T status, Gleason, diabetes)")
## N status ----
omopgenerics::logMessage("Creating N-status cohort")
N_status_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "Characterisation","N-status"), type = "csv")

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
omopgenerics::logMessage("Creating T-status cohort")
t1_status <- omopgenerics::importCodelist(here::here("..", "Codelist", "InclusionCriteria", "t1.csv"), type = "csv")
t2_status <- omopgenerics::importCodelist(here::here("..", "Codelist", "InclusionCriteria", "t2.csv"), type = "csv")
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

gleason_group_1 = c(1633550, 1634230)
gleason_group_2_3 = c(1633844, 1633826, 1635693, 1633596, 1634459)
gleason_group_4 = c(1635475, 1635079)
gleason_group_5 = c(1635009, 1633687, 1633655)


gleason_scores <- c(619648, 734332)

gleason_conceptset <- c(gleason_scores, gleason_group_1, gleason_group_2_3, gleason_group_4, gleason_group_5 )


omopgenerics::logMessage("Creating Gleason measurement cohort")
cdm$gleason <- cdm$measurement |>
  dplyr::filter(.data$measurement_concept_id %in% .env$gleason_conceptset) |>
  dplyr::select("person_id" ,"measurement_concept_id", "measurement_date", "value_as_number") |>
  dplyr::mutate(
    gleason_group = dplyr::case_when(
      .data$value_as_number <= 6 ~ "<=6",
      .data$value_as_number == 7 ~ "7",
      .data$value_as_number >= 8 & .data$value_as_number <=10 ~ "8-10",
      TRUE ~ NA_character_
      ),
    gleason_group = dplyr::case_when(
    .data$measurement_concept_id %in% .env$gleason_group_1 ~ "<=6",
    .data$measurement_concept_id %in% .env$gleason_group_2_3 ~ "7",
    .data$measurement_concept_id %in% .env$gleason_group_4 ~ "8-10",
    .data$measurement_concept_id %in% .env$gleason_group_5 ~ "8-10",
    TRUE ~ .data$gleason_group
  ),
  cohort_definition_id = 1L
  ) |>
  dplyr::select("cohort_definition_id",
                "subject_id" = "person_id",
                "cohort_start_date" = "measurement_date",
                "cohort_end_date" = "measurement_date",
                "gleason_group") |>
  dplyr::group_by(.data$subject_id, .data$cohort_start_date) |>

  dplyr::filter(
    !is.na(.data$gleason_group),
    min(.data$gleason_group, na.rm = TRUE) == max(.data$gleason_group, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::compute(name = "gleason") |>
  omopgenerics::newCohortTable()


cdm[["gleason_rwd"]] <- cdm[["gleason"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_rwd", window = c(0,  Inf), name = "gleason_rwd") |>
  CohortConstructor::requireIsLastEntry() |>
  dplyr::rename("latest_gleason_score_value" = "gleason_group")|>
  dplyr::compute(name = "gleason_rwd")




cdm[["gleason_trial"]] <- cdm[["gleason"]]|>
  CohortConstructor::requireTableIntersect(tableName = "optima_pc_trial", window = c(0,  180), name = "gleason_trial") |>
  CohortConstructor::requireIsLastEntry() |>
  dplyr::rename("latest_gleason_score_value" = "gleason_group") |>
  dplyr::compute(name = "gleason_trial" )


# diabetes ----
omopgenerics::logMessage("Building type 2 diabetes cohort")
diabetes_codelist <- omopgenerics::importCodelist(here::here("..", "Codelist", "Diabetes"), type = "csv")
diabetes <- tolower(names(diabetes_codelist))
names(diabetes_codelist) <- diabetes

cdm$type2_diabetes <- CohortConstructor::conceptCohort(cdm,
                                                       conceptSet = list("dm2_inc" = diabetes_codelist$dm2_inc),
                                                       name = "type2_diabetes") |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("dm1_prev" = diabetes_codelist$dm1_prev),
                                             window = c(-Inf, -1),
                                             intersection = c(0,0)) |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("dm2_prev" = diabetes_codelist$dm2_prev),
                                             window = c(-Inf, -1),
                                             intersection = c(0,0)) |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("antidiabetics" = diabetes_codelist$antidiabetics),
                                             window = c(-Inf, -1),
                                             intersection = c(0,0))
omopgenerics::logMessage("Finished: characteristic cohorts built")




