start_time <- Sys.time()
outputFolder <-  here::here("Results")

logfile <- file.path( paste0(outputFolder,
                             "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

omopgenerics::createLogFile(logFile = logfile)
omopgenerics::logMessage("Start time recorded.")

tableName <- c("visit_occurrence","visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence",
               "device_exposure", "measurement" , "observation")
sex <- FALSE
ageGroup <- list(c(0,17), c(18, 30), c(31, 40), c(41, 50), c(51,60), c(61, 70), c(71, 80), c(81, 90), c(91, Inf))

result <- list()

# Snapshot
omopgenerics::logMessage("Getting cdm snapshot")
result[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)


# Summarise clinical records
omopgenerics::logMessage("Summarising clinical records")
result[["clinicalRecords"]]<- OmopSketch::summariseClinicalRecords(cdm,
                                                              omopTableName = tableName,
                                                              sex = sex,
                                                              ageGroup = ageGroup)

# Summarize record counts
omopgenerics::logMessage("Summarising record counts")
result[["trend"]] <- OmopSketch::summariseTrend(cdm,
                                                event = c(tableName, "observation_period"),
                                                          output = c("record", "person", "age"),
                                                          sex = sex,
                                                          ageGroup = ageGroup,
                                                          interval = "years")

result[["person"]] <- OmopSketch::summarisePerson(cdm)
# Combine results and export


result <- omopgenerics::bind(result)

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
omopgenerics::logMessage(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

omopgenerics::exportSummarisedResult(result,
                                     minCellCount = minCellCount,
                                     path = outputFolder,
                                     fileName = "result_characterisation_{cdm_name}_{date}.csv")


