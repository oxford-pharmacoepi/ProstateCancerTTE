
if (createCohorts) {

  source(here::here("Cohorts/createCohorts.R"))

  source(here::here("Cohorts/cohortCharacterisation.R"))



 }

if (runModel) {
  source(here::here("MainStudy/Model/RunModel.R"))
  source(here::here("MainStudy/Model/RunModelMerged.R"))
}

