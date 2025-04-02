host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

minCellCount <- 5
dbName <-"gold_NCRASCR"
con <- dbConnect(drv = Postgres(),
                 dbname = "cdm_gold_p22_001867",
                 host = host,
                 port = port,
                 user = username,
                 password = password)


cdm <- cdmFromCon(con = con, cdmSchema = "public", writeSchema = "results", .softValidation = TRUE, writePrefix = "cc_", cdmName = dbName)


observation_period <-"linked"

db_filter <- "NCRASCR"

characterise_op <- FALSE

characterise_clinical_tables <- TRUE

source("DatabaseCharacterisation/databaseCharacterisaton.R")
