start_time <- Sys.time()
output_folder <- here::here("MainStudy/Results/CohortCharacterisation")
dir.create(output_folder, showWarnings = FALSE)

log_file <- file.path(output_folder, paste0(
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

logger <- log4r::create.logger(logfile = log_file, level = "INFO")

log4r::info(logger = logger, "Start time recorded.")

log4r::info(logger, "Building general conditions cohort")

conditions_codelist <- omopgenerics::importCodelist(here::here("Codelist/Characterisation/conditions"), type = "csv")

cdm$conditions <- CohortConstructor::conceptCohort(cdm, conceptSet = conditions_codelist, name = "conditions")

log4r::info(logger, "Building general medications cohort")

medications_codelist <- omopgenerics::importCodelist(here::here("Codelist/Characterisation/medications"), type = "csv")

cdm$medications <- CohortConstructor::conceptCohort(cdm, conceptSet = medications_codelist, name = "medications")

N_status_codelist <- omopgenerics::importCodelist(here::here("Codelist/Characterisation/N-status"), type = "csv")
cohorts <- c("optima_pc_trial", "optima_pc_rwd")


result <- purrr::map(cohorts, \(cohort_name){

  log4r::info(logger, paste0("Start characterisation of cohort ", cohort_name))

  log4r::info(logger, "Get counts of the cohorts.")


  count <- CohortCharacteristics::summariseCohortCount(cdm[[cohort_name]])


  log4r::info(logger, "Get atttrition")

  attrition <- CohortCharacteristics::summariseCohortAttrition(cdm[[cohort_name]])

  log4r::info(logger, "Adding latest gleason score and n status")

  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    PatientProfiles::addConceptIntersectField(conceptSet = list("gleason_score" = 619648),
                                            field = "value_as_number",
                                            indexDate = "cohort_start_date",
                                            order = "last",
                                            window = c(-Inf,0),
                                            name = cohort_name,
                                            allowDuplicates = TRUE,
                                            nameStyle = "gleason") |>
    PatientProfiles::addCategories(variable = "gleason",
                                   categories = list("latest_gleason_score_value" = list("<2" = c(0,1),
                                                                                  "2 to 6" = c(2,6),
                                                                                  "7" = c(7,7),
                                                                                  "8 to 10" = c(8,10),
                                                                                  ">10" = c(11, Inf))
                                   ),
                                   name = cohort_name) |>
    PatientProfiles::addConceptIntersectDate(conceptSet = N_status_codelist,

                                              indexDate = "cohort_start_date",
                                              order = "last",
                                              window = c(-Inf,0),
                                              name = cohort_name,
                                             nameStyle = "{concept_name}"
                                             ) |>
   dplyr::mutate(
     n_date = pmax(n0, nx, n1, n2, n3, na.rm = TRUE),
     latest_n_status = dplyr::case_when(
       n0 == n_date ~ "n0",
       nx == n_date ~ "nx",
       n1 == n_date ~ "n1",
       n2 == n_date ~ "n2",
       n3 == n_date ~ "n3",
       TRUE ~ NA_character_
     )
   ) %>%
   dplyr::group_by(subject_id) %>%
   dplyr::filter(n_date == max(n_date, na.rm = TRUE)) %>%
   dplyr::ungroup() %>%
   dplyr::select(-n0, -nx, -n1, -n2, -n3) |> dplyr::compute(name = cohort_name) |>
  dplyr::mutate(
    missing_psa_value =
      dplyr::if_else(is.na(.data$psa_value), "Yes", "No")
    )



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
  ),
  otherVariables = c("latest_gleason_score_value", "latest_n_status", "psa_value", "missing_psa_value")
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

log4r::info(logger, "Attrition prostate cancer between age 50 and 69 trial cohort")
result <- result |> omopgenerics::bind(
  CohortCharacteristics::summariseCohortAttrition(cdm[["prostate_cancer_age_50_69"]])
)


omopgenerics::exportSummarisedResult(result, fileName =  paste0("prostateCancer_characteristics_{cdm_name}_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".csv"), path = output_folder )



dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log4r::info(logger,paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

