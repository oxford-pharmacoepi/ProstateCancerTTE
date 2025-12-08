
if (createCohorts) {

  source(here::here("..", "Cohorts", "createCohorts.R"))

  source(here::here("..", "Cohorts", "characteristicsCohorts.R"))


  source(here::here("..", "Cohorts", "cohortCharacterisation.R"))



 }

if (runModel) {
  source(here::here("Model/RunModel.R"))
  source(here::here("Model/RunModelMerged.R"))
}

