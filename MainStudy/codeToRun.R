host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

minCellCount <- 5
dbName <-"aurum_pc"
con <- dbConnect(drv = Postgres(),
                 dbname = "cdm_aurum_p22_001867",
                 host = host,
                 port = port,
                 user = username,
                 password = password)


createCohorts <- TRUE
runModel <- TRUE

cdm <- cdmFromCon(con = con,
                  cdmSchema = "public",
                  writeSchema = "results",
                  achillesSchema = "results" ,
                  .softValidation = TRUE,
                  writePrefix = "cc_",
                  cdmName = dbName,
                  cohortTables = c("optima_pc_trial", "optima_pc_rwd")[!createCohorts]

)

cdm$observation_period<- cdm$observation_period |>
  dplyr::filter(.data$period_type_concept_id == 32882)

source("MainStudy/RunAnalysis.R")



CDMConnector::cdmDisconnect(cdm)

