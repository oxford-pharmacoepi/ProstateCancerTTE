# Start
start_time <- Sys.time()
outputFolder <-  here::here("Results/DatabaseCharacterisation")

logfile <- file.path( paste0(outputFolder,
                             "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = logfile, append = TRUE)
  cli::cli_inform(paste(Sys.time(), "-", message, "\n"))
}

log_message("Start time recorded.")


sex <- TRUE # FALSE
ageGroup <- list(c(0,17), c(18, 30), c(31, 40), c(41, 50), c(51,60), c(61, 70), c(71, 80), c(81, 90), c(91, Inf))

dateRange <- as.Date(c("2010-01-01", NA))

# Snapshot
log_message("Getting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm)

# Population Characteristics
log_message("Getting population characteristics")

cdm <- omopgenerics::bind(
  CohortConstructor::demographicsCohort(cdm, "population_1", sex = "Both"),
  CohortConstructor::demographicsCohort(cdm, "population_2", sex = "Both", ageRange = ageGroup),
  name = "population"
)


set <- omopgenerics::settings(cdm$population) |>
  dplyr::mutate(cohort_name = tolower(dplyr::if_else(
    is.na(.data$age_range), "general_population", paste0("age_group_", .data$age_range)
  ))) |>
  dplyr::select("cohort_definition_id", "cohort_name")

result_populationCharacteristics <- cdm$population |>
  omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE) |>
  CohortConstructor::trimToDateRange(dateRange = dateRange) |>
  PatientProfiles::addSexQuery() |>
  CohortCharacteristics::summariseCharacteristics(
    strata = list("sex"),
    estimates = list(
      date = c("min", "q25", "median", "q75", "max"),
      numeric = c("min", "q25", "median", "q75", "max", "mean", "sd", "density"),
      categorical = c("count", "percentage"),
      binary = c("count", "percentage")
    )
  )

omopgenerics::dropSourceTable(cdm = cdm, c("population_1", "population_2", "population"))


# Summarize in observation records
log_message("Summarising in observation records and person-days")
result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period,
                                                           output = c("records","person-days"),
                                                           interval = "years",
                                                           sex = sex,
                                                           ageGroup = ageGroup,
                                                           dateRange = dateRange)




# Summarise observation period
log_message("Summarising observation period")
result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period,
                                                                   sex = sex,
                                                                   ageGroup = ageGroup,
                                                                   dateRange = dateRange)


# Combine results and export
result <- omopgenerics::bind(snapshot, result_populationCharacteristics, result_inObservation, result_observationPeriod)
omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, path = outputFolder, fileName = paste0(
  "result_characterisation_observation_period_", dbName, ".csv"))



# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))


