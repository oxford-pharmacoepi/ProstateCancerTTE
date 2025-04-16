host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

minCellCount <- 5
dbName <-"gold_p22_00186"
con <- dbConnect(drv = Postgres(),
                 dbname = "cdm_gold_p22_001867",
                 host = host,
                 port = port,
                 user = username,
                 password = password)


cdm <- cdmFromCon(con = con, cdmSchema = "public", writeSchema = "results", achillesSchema = "results" , .softValidation = TRUE, writePrefix = "cc_", cdmName = dbName)


observation_period <-"linked"

db_filter <- "NCRASCR"

characterise_op <- FALSE

characterise_clinical_tables <- FALSE

diagnostics <- TRUE

if (characterise_op || characterise_clinical_tables) {

source("DatabaseCharacterisation/databaseCharacterisaton.R")

}

if (diagnostics){

  cdm$observation_period<- cdm$observation_period |>
    dplyr::filter(.data$period_type_concept_id == 32882)

  source("Diagnostics/runDiagnostics.R")

}
