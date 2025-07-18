if (observation_period == "linked") {

  cdm$observation_period <- cdm$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)

} else if (observation_period == "primary_care") {
  cdm$observation_period <- cdm$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32880)
}



# Database Characterisation

if (characterise_op) {
  source("DatabaseCharacterisation/observationPeriodCharacterisation.R")
}

if (characterise_clinical_tables) {

  if (db_filter == "primary_care") {
    cdm$condition_occurrence <- cdm$condition_occurrence |>
      dplyr::filter(.data$condition_type_concept_id == 32817)

    cdm$visit_detail <- cdm$visit_detail |>
      dplyr::filter(.data$visit_detail_concept_id == 581477 & .data$visit_detail_type_concept_id == 32817)

    cdm$visit_occurrence <- cdm$visit_occurrence |>
      dplyr::filter(.data$visit_concept_id == 581477 & .data$visit_type_concept_id == 32817)

    cdm$death <- cdm$death |>
      dplyr::filter(.data$death_type_concept_id == 32817)

  } else if (db_filter == "hes") {

    cdm$condition_occurrence <- cdm$condition_occurrence |>
      dplyr::filter(.data$condition_type_concept_id == 32829)

    cdm$visit_detail <- cdm$visit_detail |>
      dplyr::filter(.data$visit_detail_concept_id %in% c(9201, 32037)  & .data$visit_detail_type_concept_id == 32818 )

    cdm$visit_occurrence <- cdm$visit_occurrence |>
      dplyr::filter(.data$visit_concept_id == 9201 & .data$visit_type_concept_id == 32818 )

    cdm$death <- cdm$death |>
      dplyr::filter(.data$death_type_concept_id == 32829)

  }  else if (db_filter == "NCRASCR") {

    cdm$condition_occurrence <- cdm$condition_occurrence |>
      dplyr::filter(.data$condition_type_concept_id %in% c(32815, 32828, 32835, 32879) )

    cdm$visit_detail <- cdm$visit_detail |>
      dplyr::filter(.data$visit_detail_concept_id == 38004268   & .data$visit_detail_type_concept_id == 32879 )

    cdm$visit_occurrence <- cdm$visit_occurrence |>
      dplyr::filter(.data$visit_concept_id == 38004268  & .data$visit_type_concept_id == 32879 )

    cdm$death <- cdm$death |>
      dplyr::filter(.data$death_type_concept_id %in% c(32815, 32828, 32835, 32879))
  }

  cdm$drug_exposure <- cdm$drug_exposure |>
    dplyr::semi_join(cdm$visit_occurrence, by = "visit_occurrence_id")

  cdm$procedure_occurrence <- cdm$procedure_occurrence |>
    dplyr::semi_join(cdm$visit_occurrence, by = "visit_occurrence_id")

  cdm$device_exposure <- cdm$device_exposure |>
    dplyr::semi_join(cdm$visit_occurrence, by = "visit_occurrence_id")

  cdm$measurement <- cdm$measurement |>
    dplyr::semi_join(cdm$visit_occurrence, by = "visit_occurrence_id")

  cdm$observation <- cdm$observation |>
    dplyr::semi_join(cdm$visit_occurrence, by = "visit_occurrence_id")


  source("DatabaseCharacterisation/clinicalTablesCharacterisation.R")
}



CDMConnector::cdmDisconnect(cdm)
