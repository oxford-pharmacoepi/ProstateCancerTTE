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


cohorts <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf", "optima_pc_trial_2010_2020", "optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")
# cohorts <- c( "optima_pc_rwd_matched",  "merged_optima_pc_trial_matched", "merged_optima_pc_rwd_matched","optima_pc_rwd_50_69_2010_2020_matched",
#              "optima_pc_rwd_2010_2020_matched", "optima_pc_rwd_50_69_2010_2020_matched", "optima_pc_rwd_70_inf_2010_2020_matched",
#              "merged_optima_pc_trial_2010_2020_matched", "merged_optima_pc_rwd_2010_2020_matched", "merged_optima_pc_rwd_50_69_2010_2020_matched", "merged_optima_pc_rwd_70_inf_2010_2020_matched")

#cohorts <- c("optima_pc_rwd_50_69_matched", "optima_pc_rwd_70_inf_matched", "merged_optima_pc_rwd_50_69_matched", "merged_optima_pc_rwd_70_inf_matched")

#cohorts <- c("optima_pc_trial","merged_optima_pc_trial_matched")

result <- purrr::map(cohorts, \(cohort_name){

  log4r::info(logger, paste0("Start characterisation of cohort ", cohort_name))

  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    CohortConstructor::renameCohort(cohortId = 1, newCohortName = paste0("ebrt_", cohort_name)) |>
    CohortConstructor::renameCohort(cohortId = 2, newCohortName = paste0("rp_", cohort_name))

  log4r::info(logger, "Get counts of the cohorts.")


  count <- CohortCharacteristics::summariseCohortCount(cdm[[cohort_name]])


  log4r::info(logger, "Get atttrition")


  attrition <- CohortCharacteristics::summariseCohortAttrition(cdm[[cohort_name]])

  log4r::info(logger, "Adding latest gleason score and n status")


  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    dplyr::select(!dplyr::any_of(c("latest_gleason_score_value", "psa_value", "latest_psa_value", "latest_n_status"))) |>
    dplyr::compute() |>
    dplyr::left_join(cdm[["gleason"]] |> dplyr::select("subject_id","latest_gleason_score_value" ),  by = "subject_id") |>
    dplyr::mutate(
      latest_gleason_score_value = dplyr::coalesce(.data$latest_gleason_score_value, "missing")
    ) |>
    dplyr::left_join(cdm[["n_status"]] |> dplyr::select("subject_id","latest_n_status"), by = "subject_id") |>
    dplyr::mutate(
      latest_n_status = dplyr::coalesce(.data$latest_n_status, "missing")
    ) |>
    dplyr::left_join(cdm[["t_status"]] |> dplyr::select("subject_id","latest_t_status"), by = "subject_id") |>
    dplyr::mutate(
      latest_t_status = dplyr::coalesce(.data$latest_t_status, "missing")
    ) |>
    dplyr::left_join(cdm[["psa_values"]] |> dplyr::select("subject_id","psa_value", "latest_psa_value")) |>

    dplyr::mutate(
      latest_psa_value = dplyr::coalesce(.data$latest_psa_value, "missing")
    ) |>
    dplyr::compute(name = cohort_name)




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
  otherVariables = c("latest_gleason_score_value", "latest_n_status", "psa_value", "latest_psa_value")
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



  res <- omopgenerics::bind(count, characteristics, lsc, attrition, overlap)
  #res <- omopgenerics::bind(count, characteristics, lsc)
  omopgenerics::exportSummarisedResult(res, fileName =  paste0("prostateCancer_characteristics_{cdm_name}_", cohort_name,".csv"), path = output_folder )



}) |>
  omopgenerics::bind()

log4r::info(logger, "Attrition prostate cancer between age 50 and 69 trial cohort")
pc_cohort_caharcterisation <- CohortCharacteristics::summariseCohortAttrition(cdm[["prostate_cancer_age_50_69"]])


omopgenerics::exportSummarisedResult(pc_cohort_caharcterisation, fileName =  paste0("prostateCancer_characteristics_cohorts_{cdm_name}_prostate_cancer_age_50_69.csv"), path = output_folder )

dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log4r::info(logger,paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

