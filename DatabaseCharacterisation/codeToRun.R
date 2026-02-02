host = Sys.getenv("HOST")
port = Sys.getenv("PORT")
username = Sys.getenv("USER")
password = Sys.getenv("PASSWORD")


library(DBI)
library(RPostgres)
library(CDMConnector)

minCellCount <- 5

dbName <-"gold_rtds"

con <- dbConnect(drv = Postgres(),
                 dbname = "cdm_gold_p22_001867_rtds",
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

db_filter <- "RTDS"

characterise_op <- FALSE

characterise_clinical_tables <- TRUE

source("databaseCharacterisation.R")



