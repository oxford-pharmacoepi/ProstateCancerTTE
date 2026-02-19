output_directory <- here::here("Results")

log_file <- file.path(output_directory, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))
omopgenerics::createLogFile(logFile = log_file)

omopgenerics::logMessage("Instatiating cohorts")
source(here::here("Scripts", "createCohorts.R"))
  
source(here::here("Scripts","characteristicsCohorts.R"))
  
omopgenerics::logMessage("Summarising cohorts' characteristics")
source(here::here("Scripts", "cohortCharacterisation.R"))
  