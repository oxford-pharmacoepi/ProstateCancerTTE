start_time <- Sys.time()
output_folder <- here::here("Results/CohortCharacterisation")
dir.create(output_folder, showWarnings = FALSE)

log_file <- file.path(output_folder, paste0(
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

logger <- log4r::create.logger(logfile = log_file, level = "INFO")

log4r::info(logger = logger, "Start time recorded.")

log4r::info(logger, "Building general conditions cohort")

conditions_codelist <- omopgenerics::importCodelist(here::here("Cohorts/CodelistFlag/conditions"), type = "csv")

cdm$conditions <- CohortConstructor::conceptCohort(cdm, conceptSet = conditions_codelist, name = "conditions")

log4r::info(logger, "Building general medications cohort")

medications_codelist <- omopgenerics::importCodelist(here::here("Cohorts/CodelistFlag/medications"), type = "csv")

cdm$medications <- CohortConstructor::conceptCohort(cdm, conceptSet = conditions_codelist, name = "medications")

cohorts <- c("optima_pc_trial", "optima_pc_rwd")


result <- purrr::map(cohorts, \(cohort_name){

  log4r::info(logger, paste0("Start characterisation of cohort ", cohort_name))

  log4r::info(logger, "Get counts of the cohorts.")


  count <- CohortCharacteristics::summariseCohortCount(cdm[[cohort_name]])


  log4r::info(logger, "Get atttrition")

  attrition <- CohortCharacteristics::summariseCohortAttrition(cdm[[cohort_name]])

  log4r::info(logger, "Cohort characterisation")

  characteristics <- CohortCharacteristics::summariseCharacteristics(cdm[[cohort_name]], cohortIntersectFlag = list(
    "Conditions any time prior" = list(
      targetCohortTable = "conditions", window = c(-Inf, -1)
    ),
    "Medications in the prior year" = list(
      targetCohortTable = "medications", window = c(-365, -1)
    )
  ),
  tableIntersectCount = list(
    "Number visits prior year" = list(
      tableName = "visit_occurrence", window = c(-365, -1)
    )
  )
  )

  log4r::info(logger, "Large scale characterisation")

  lsc <- CohortCharacteristics::summariseLargeScaleCharacteristics(cdm[[cohort_name]],
                                                                   eventInWindow = c("condition_occurrence", "observation", "procedure_occurrence", "device_exposure"),
                                                                   episodeInWindow = "drug_exposure",
                                                                   window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365), c(366, Inf)),
                                                                   minimumFrequency = 0.0
  )



  log4r::info(logger, "Get overlap")
  overlap <- CohortCharacteristics::summariseCohortOverlap(cdm[[cohort_name]])



  res <- omopgenerics::bind(count, attrition, overlap, characteristics, lsc)



}) |>
  omopgenerics::bind()


omopgenerics::exportSummarisedResult(result, fileName =  paste0("prostateCancer_characteristics_{cdm_name}_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".csv"), path = here::here("Results/CohortCharacterisation")  )



dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log4r::info(logger,paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

