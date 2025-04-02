start_time <- Sys.time()
outputFolder <-  here::here("DatabaseCharacterisation/ClinicalTables")

logfile <- file.path( paste0(outputFolder,
                             "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = logfile, append = TRUE)
  cli::cli_inform(paste(Sys.time(), "-", message, "\n"))
}

log_message("Start time recorded.")

tableName <- c("visit_occurrence","visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence",
               "device_exposure", "measurement" , "observation", "death")
sex <- TRUE # FALSE
ageGroup <- list(c(0,17), c(18, 30), c(31, 40), c(41, 50), c(51,60), c(61, 70), c(71, 80), c(81, 90), c(91, Inf))

dateRange <- as.Date(c("2010-01-01", NA))

# Snapshot
log_message("Getting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm)


# Summarise missing data
log_message("Summarising missing data")
result_missingData <- OmopSketch::summariseMissingData(cdm ,
                                                       omopTableName = tableName,
                                                       sex = sex,
                                                       ageGroup = ageGroup,
                                                       interval = "years",
                                                       dateRange = dateRange)




# Summarise concept counts
log_message("Summarising concept id counts")
result_conceptIdCount <- OmopSketch::summariseConceptIdCounts(cdm,
                                                              omopTableName = tableName,
                                                              sex = sex,
                                                              ageGroup = ageGroup,
                                                              interval = "years",
                                                              dateRange = dateRange)

# Summarise clinical records
log_message("Summarising clinical records")
result_clinicalRecords<- OmopSketch::summariseClinicalRecords(cdm,
                                                              omopTableName = tableName,
                                                              sex = sex,
                                                              ageGroup = ageGroup,
                                                              dateRange = dateRange)

# Summarize record counts
log_message("Summarising record counts")
result_recordCounts <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                        sex = sex,
                                                        ageGroup = ageGroup,
                                                        interval = "years",
                                                        dateRange = dateRange)


# log_message("Summarising missing data - person table")
#
# result_missingDataPerson <- OmopSketch::summariseMissingData(cdm,
#                                                              omopTableName = "person")
# Combine results and export
result <- omopgenerics::bind(snapshot, result_missingData, result_conceptIdCount, result_clinicalRecords, result_recordCounts)
omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, path = outputFolder, fileName = paste0(
  "result_characterisation_", dbName, ".csv"))



# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

