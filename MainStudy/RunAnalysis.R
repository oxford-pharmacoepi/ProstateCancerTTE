output_directory <- here::here("Results")

log_file <- file.path(output_directory, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))
omopgenerics::createLogFile(logFile = log_file)


if (createCohorts) {
  omopgenerics::logMessage("Instatiating cohorts")
  source(here::here("..", "Cohorts", "Scripts", "createCohorts.R"))

  source(here::here("..", "Cohorts", "Scripts", "characteristicsCohorts.R"))

  omopgenerics::logMessage("Summarising cohorts' characteristics")
  source(here::here("..", "Cohorts", "Scripts", "cohortCharacterisation.R"))


 }

if (runModel) {
  omopgenerics::logMessage("Running model")
  source(here::here("Model", "RunModel.R"))

}

