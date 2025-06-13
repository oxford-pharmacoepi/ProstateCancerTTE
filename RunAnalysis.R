if (characterise_op || characterise_clinical_tables) {

  source("DatabaseCharacterisation/databaseCharacterisaton.R")

}

if (diagnostics){

  cdm$observation_period<- cdm$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)

  source("CodelistDiagnostics/runDiagnostics.R")

}


if (cohorts) {

  cdm$observation_period<- cdm$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)

  source(here::here("Cohorts/createCohorts.R"))

  source(here::here("Cohorts/cohortCharacterisation.R"))



}

if (propensity_scores) {

  source(here::here("PropensityScores/getPropensityScores.R"))

}

