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

cdm <- cdmFromCon(con = con,
                  cdmSchema = "public",
                  writeSchema = "results",
                  achillesSchema = "results" ,
                  .softValidation = TRUE,
                  writePrefix = "cc_",
                  cdmName = dbName)



observation_period <-"linked"

db_filter <- "NCRASCR"

characterise_op <- TRUE

characterise_clinical_tables <- FALSE

source("DatabaseCharacterisation/databaseCharacterisation.R")



