renv::restore()
dbName <- "..."

con <- DBI::dbConnect("...")

cdmSchema <- "..."
writeSchema <- "..."
achillesSchema <- "..."
prefix <- "..."

minCellCount <- 5

cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema,
                                writeSchema = c(schema = writeSchema,
                                                prefix = prefix),
                                achillesSchema = achillesSchema,
                                cdmName = dbName)

# cdm$observation_period<- cdm$observation_period |>
#   dplyr::filter(.data$period_type_concept_id == 32882) uncomment if running in CPRD

fullDiagnostics <- TRUE

source("RunDiagnostics.R")


CDMConnector::cdmDisconnect(cdm)
