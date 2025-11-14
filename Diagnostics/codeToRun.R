renv::restore()
dbName <- "..."

con <- DBI::dbConnect("...")

cdmSchema <- "..."
writeSchema <- "..."

prefix <- "..."

minCellCount <- 5

cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema,
                                writeSchema = c(schema = writeSchema,
                                                prefix = prefix),
                                cdmName = dbName)

# cdm$observation_period<- cdm$observation_period |>
#   dplyr::filter(.data$period_type_concept_id == 32882) uncomment if running in CPRD

source("RunDiagnostics.R")



CDMConnector::cdmDisconnect(cdm)
