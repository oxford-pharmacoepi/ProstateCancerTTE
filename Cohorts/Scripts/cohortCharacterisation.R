
source("~/ProstateCancerTTE/Cohorts/Scripts/functions.R")

omopgenerics::logMessage("Building general conditions cohort")
conditions_codelist <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/Characterisation/conditions", type = "csv")

cdm$conditions <- CohortConstructor::conceptCohort(cdm, conceptSet = conditions_codelist, name = "conditions")

omopgenerics::logMessage("Building general medications cohort")

medications_codelist <- omopgenerics::importCodelist("~/ProstateCancerTTE/Codelist/Characterisation/medications", type = "csv")

cdm$medications <- CohortConstructor::conceptCohort(cdm, conceptSet = medications_codelist, name = "medications")


cohorts <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf", "optima_pc_trial_2010_2020", "optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")


result <- purrr::map(cohorts, \(cohort_name){

  omopgenerics::logMessage(paste0("Start characterisation of cohort ", cohort_name))

  cdm[[cohort_name]] <- cdm[[cohort_name]] |>
    CohortConstructor::renameCohort(cohortId = 1, newCohortName = paste0("ebrt_", cohort_name)) |>
    CohortConstructor::renameCohort(cohortId = 2, newCohortName = paste0("rp_", cohort_name)) |>
    addCharacteristics()

  omopgenerics::logMessage("Get counts of the cohorts.")


  count <- CohortCharacteristics::summariseCohortCount(cdm[[cohort_name]])


  omopgenerics::logMessage("Get atttrition")


  attrition <- CohortCharacteristics::summariseCohortAttrition(cdm[[cohort_name]])

  omopgenerics::logMessage("Cohort characterisation")

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
  otherVariables = c("latest_gleason_score_value", "latest_n_status", "psa_value", "latest_t_status", "latest_psa_value")
  )

  omopgenerics::logMessage("Large scale characterisation")

  lsc <- CohortCharacteristics::summariseLargeScaleCharacteristics(cdm[[cohort_name]],
                                                                   eventInWindow = c("condition_occurrence", "observation", "procedure_occurrence", "device_exposure"),
                                                                   episodeInWindow = "drug_exposure",
                                                                   window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365), c(366, Inf)),
                                                                   minimumFrequency = 0.0
  )



  omopgenerics::logMessage("Get overlap")
  overlap <- CohortCharacteristics::summariseCohortOverlap(cdm[[cohort_name]])

  omopgenerics::bind(count, characteristics, lsc, attrition, overlap)
 

})

omopgenerics::logMessage("Attrition prostate cancer between age 50 and 69 trial cohort")
result[["prostate_cancer_age_50_69"]] <- CohortCharacteristics::summariseCohortAttrition(cdm[["prostate_cancer_age_50_69"]])

result <- omopgenerics::bind(result)
omopgenerics::exportSummarisedResult(result, fileName =  paste0("prostateCancer_characterisation_cohorts_{cdm_name}.csv"), path = output_folder )

