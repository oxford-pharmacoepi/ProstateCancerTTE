# Start
start_time <- Sys.time()
outputFolder <-  here::here("Results/DatabaseCharacterisation")

logfile <- file.path( paste0(outputFolder,
                             "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

omopgenerics::createLogFile(logFile = logfile)


omopgenerics::logMessage("Start time recorded.")

sex <- FALSE
ageGroup <- list(c(0,17), c(18, 30), c(31, 40), c(41, 50), c(51,60), c(61, 70), c(71, 80), c(81, 90), c(91, Inf))

dateRange <- NULL

result <- list()
# Snapshot
omopgenerics::logMessage("Getting cdm snapshot")
result[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)

# Population Characteristics
omopgenerics::logMessage("Getting population characteristics")

cdm <- omopgenerics::bind(
  CohortConstructor::demographicsCohort(cdm, "population_1", sex = "Male"),
  CohortConstructor::demographicsCohort(cdm, "population_2", sex = "Male", ageRange = ageGroup),
  name = "population"
)


set <- omopgenerics::settings(cdm$population) |>
  dplyr::mutate(cohort_name = tolower(dplyr::if_else(
    is.na(.data$age_range), "general_population", paste0("age_group_", .data$age_range)
  ))) |>
  dplyr::select("cohort_definition_id", "cohort_name")

result[["populationCharacteristics"]] <- cdm$population |>
  omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE) |>
  CohortConstructor::trimToDateRange(dateRange = dateRange) |>
  CohortCharacteristics::summariseCharacteristics(
    estimates = list(
      date = c("min", "q25", "median", "q75", "max"),
      numeric = c("min", "q25", "median", "q75", "max", "mean", "sd", "density"),
      categorical = c("count", "percentage"),
      binary = c("count", "percentage")
    )
  )

omopgenerics::dropSourceTable(cdm = cdm, c("population_1", "population_2", "population"))


# Summarize in observation records
omopgenerics::logMessage("Summarising in observation records and person-days")
result[["trend"]] <- OmopSketch::summariseTrend(cdm,
                                                episode = "observation_period",
                                                output = c("records","person-days", "age"),
                                                interval = "years",
                                                sex = sex,
                                                ageGroup = ageGroup,
                                                dateRange = dateRange)




# Summarise observation period
omopgenerics::logMessage("Summarising observation period")
result[["observationPeriod"]] <- OmopSketch::summariseObservationPeriod(cdm,
                                                                   sex = sex,
                                                                   ageGroup = ageGroup,
                                                                   dateRange = dateRange)

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
omopgenerics::logMessage(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))


# Combine results and export
result <- omopgenerics::bind(result)
omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, path = outputFolder, fileName = paste0(
  "result_characterisation_observation_period_", dbName, ".csv"))



