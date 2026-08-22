
library(RPostgres)
library(CDMConnector)
library(omopgenerics, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(here)
library(CohortConstructor)
library(PatientProfiles)
library(glmnet)
library(purrr)
library(nnet)
library(cli)
library(clock)
library(survival)
library(stringr)
library(readr)
library(OmopSketch)
library(CodelistGenerator)
library(CohortCharacteristics)
library(splines)
library(duckdb)
library(modi)

# create log file
logFile <- here("Results", "log_{date}_{time}.txt")
createLogFile(logFile = logFile)

# source functions
source(here("functions.R"))

# create cdm object
logMessage("Create cdm object")
#cdmName <- "CPRD Aurum"
cdmName <- "CPRD GOLD"

con <- dbConnect(drv = Postgres(),
                 dbname = if_else(cdmName == "CPRD GOLD", "cdm_gold_p22_001867", "cdm_aurum_p22_001867"),
                 host = Sys.getenv("DB_HOST"),
                 port = Sys.getenv("DB_PORT"),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

cdm <- cdmFromCon(
  con = con,
  cdmName = cdmName,
  cdmSchema = "public",
  writeSchema = "results",
  writePrefix = "mc_", 
  .softValidation = TRUE
)

# filter observation_period
logMessage("Filter observation periods")
cdm$observation_period <- cdm$observation_period |>
  filter(period_type_concept_id == 32882)

# empty results list
results <- list()

# snapshot
logMessage("Extract snapshot")
results$snapshot <- summariseOmopSnapshot(cdm = cdm)

# observation period
logMessage("Summarise observation period")
results$obs_period <- summariseObservationPeriod(cdm = cdm)

source(here("Analyses", "InstantiateCohorts.R"))

logMessage("Characterise cohorts")
source(here("Analyses", "Characterisation.R"))

logMessage("Apply censor logic")
source(here("Analyses", "ApplyCensorLogic.R"))

logMessage("Calculate weights")
source(here("Analyses", "CalculateWeights.R"))

logMessage("Confounding over time")
source(here("Analyses", "SummariseFollowUp.R"))

logMessage("Confounding over time")
source(here("Analyses", "ConfoundingOvertime.R"))

logMessage("Outcome model")
source(here("Analyses", "OutcomeModel.R"))

# export results
exportSummarisedResult(
  bind(results),
  path = here("Results"),
  fileName = "results_all_{cdm_name}.csv"
)

# drop created tables
# dropSourceTable(cdm = cdm, name = everything())
