omopgenerics::logMessage("=== Instatiating Cohorts: Start ===")

# import concepts ----

folder_path <-here::here("..", "Codelist", "InclusionCriteria")

omopgenerics::logMessage(paste0("Reading codelists from: ", folder_path))

codelist <- omopgenerics::importCodelist(paste0(folder_path), "csv")

codelist_treatment <- list("ebrt" = codelist$ebrt, "radical_prostatectomy" = codelist$radical_prostatectomy)

codelist_early_stage <- list("t1_t2" = c(codelist$t1, codelist$t2) , "stage1_2" = codelist$stage1_2)

codelist_advanced_stage <- list("t3_t4" =  codelist$t3_t4 , "stage3_4" = codelist$stage3_4)



if (grepl("gold", dbName)) {
  dir_excluded_subjects <- here::here("..", "SubjectsToRemove", "cdm_gold_p22_001867_person_id_all.csv")
  excluded_subjects <- utils::read.csv(dir_excluded_subjects, header = TRUE)$person_id
} else if (grepl("aurum", dbName)) {
  dir_excluded_subjects <- here::here("..", "SubjectsToRemove", "cdm_aurum_p22_001867_person_id_all.csv")
  excluded_subjects <- utils::read.csv(dir_excluded_subjects, header = TRUE)$person_id
} else {
  excluded_subjects <- character(0)
}


# concept cohorts ----

omopgenerics::logMessage("Building age restricted prostate cancer cohort (50–69)")

cdm$prostate_cancer_age_50_69 <- CohortConstructor::conceptCohort(cdm,
                                                                  conceptSet = list("prostate_cancer" = codelist$prostate_cancer),
                                                                  name = "prostate_cancer_age_50_69" ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireDemographics(sex = "Male",
                                         ageRange = list(c(50, 69)),
                                         minPriorObservation = 365,
                                         indexDate = "cohort_start_date")
omopgenerics::logMessage("Building PSA measurements cohort")

cdm[["psa_values"]] <- cdm$measurement |>
  dplyr::filter(.data$measurement_concept_id %in% codelist$psa) |>
  dplyr::select("person_id" , "measurement_date", "value_as_number") |>
  dplyr::group_by(.data$person_id, .data$measurement_date) |>
  dplyr::summarise(
    min_val         = min(.data$value_as_number, na.rm = TRUE),
    max_val         = max(.data$value_as_number, na.rm = TRUE),
    all_na          = dplyr::if_else(sum(as.integer(!is.na(.data$value_as_number))) == 0L,TRUE, FALSE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    value_as_number = dplyr::case_when(
      all_na ~ NA_real_,
      min_val == max_val ~ min_val,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(.data$value_as_number)) |>
  dplyr::select("subject_id" = "person_id", "cohort_start_date" = "measurement_date", "cohort_end_date" = "measurement_date",
                "psa_value" = "value_as_number"
  )|>
  dplyr::mutate("cohort_definition_id" = 1L) |>
  PatientProfiles::filterInObservation(indexDate = "cohort_start_date") |>
  dplyr::compute(name = "psa_values") |>
  omopgenerics::newCohortTable(cohortSetRef = tibble::tibble(cohort_definition_id = 1,
                                                             cohort_name = "psa_values"))

cdm[["psa_values_rwd"]] <- cdm[["psa_values"]] |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = unname(unlist(codelist_treatment))),
                                             name = "psa_values_rwd",
                                             window = c(0, 180) ) |>
  CohortConstructor::requireIsLastEntry() |>
  PatientProfiles::addCategories(variable = "psa_value", categories = list("latest_psa_value" = list("<3" = c(0, 2.99),
                                                                                                     "3 to 5.99" = c(3, 5.99),
                                                                                                     "6 to 9.99" = c(6, 9.99),
                                                                                                     "10 to 19.99" = c(10, 19.99),
                                                                                                     "20 to 39.99" = c(20, 39.99),
                                                                                                     ">40" = c(40, Inf)
  )
  ),
  name = "psa_values_rwd")

cdm[["psa_values_trial"]] <- cdm[["psa_values"]] |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("treatment" = unname(unlist(codelist_treatment))),
                                             name = "psa_values_trial",
                                             window = c(0, 180) ) |>
  CohortConstructor::requireIsLastEntry() |>
  PatientProfiles::addCategories(variable = "psa_value", categories = list("latest_psa_value" = list("<3" = c(0, 2.99),
                                                                                                     "3 to 5.99" = c(3, 5.99),
                                                                                                     "6 to 9.99" = c(6, 9.99),
                                                                                                     "10 to 19.99" = c(10, 19.99),
                                                                                                     "20 to 39.99" = c(20, 39.99),
                                                                                                     ">40" = c(40, Inf)
                                                                                                    )
  ),
  name = "psa_values_trial")


cdm$psa_trial <- cdm$psa_values_trial |>
  dplyr::filter(.data$psa_value <= 19.99 & .data$psa_value >=3) |>
  dplyr::compute(name = "psa_trial")

omopgenerics::logMessage("Building early-stage cohort")
cdm$early_stage <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_early_stage, name = "early_stage") |>
  PatientProfiles::addConceptIntersectDate(conceptSet = list("treatment" = unname(unlist(codelist_treatment))), nameStyle = "treatment_date", name = "early_stage")

cdm$early_stage_trial <- cdm$early_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m0" = codelist$m0),
                                             window = c(-180, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t1_t2",
                                             intersections = c(1, Inf),
                                             name = "early_stage_trial"
                                             ) |>
  CohortConstructor::unionCohorts(name = "early_stage_trial")

cdm$early_stage_rwd <- cdm$early_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m0" = codelist$m0),
                                             window = c(-Inf, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t1_t2",
                                             intersections = c(1, Inf),
                                             name = "early_stage_rwd"
  ) |>
  CohortConstructor::unionCohorts(name = "early_stage_rwd")

omopgenerics::logMessage("Building advanced-stage cohort")

cdm$advanced_stage <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_advanced_stage, name = "advanced_stage") |>
  PatientProfiles::addConceptIntersectDate(conceptSet = list("treatment" = unname(unlist(codelist_treatment))), nameStyle = "{concept_name}_date", name = "advanced_stage")

cdm$advanced_stage_trial <- cdm$advanced_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m1" = codelist$m1),
                                             window = c(-180, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t3_t4",
                                             intersections = c(1, Inf),
                                             name = "advanced_stage_trial") |>
  CohortConstructor::unionCohorts( name = "advanced_stage_trial")

cdm$advanced_stage_rwd <- cdm$advanced_stage |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("m1" = codelist$m1),
                                             window = c(-Inf, 0),
                                             indexDate = "treatment_date",
                                             cohortId = "t3_t4",
                                             intersections = c(1, Inf),
                                             name = "advanced_stage_rwd") |>
  CohortConstructor::unionCohorts( name = "advanced_stage_rwd")


# tte ----
omopgenerics::logMessage("Instatiating trial cohort with all eligibility checks")
cdm$optima_pc_trial <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_trial") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("rp_to_exclude" = codelist$rp_exclude),
                                             window = c(-Inf, 0),
                                             intersection = c(0,0),
                                             cohortId = "radical_prostatectomy") |>

  CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_age_50_69",
                                            window = c(-180,0),
                                            intersections = c(1, Inf)) |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("pc_to_exclude" = codelist$prostate_cancer_exclude),
                                             window = c(-Inf, 0),
                                             intersection = c(0,0)) |>

  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "early_stage_trial",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_start_date",
  ) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "advanced_stage_trial",
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_start_date",
  ) |>
  CohortConstructor::requireCohortIntersect(
    targetCohortTable = "psa_trial",
    window = c(-180, 0),
    intersections = c(1, Inf),
    cohortId = NULL,
    targetCohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_start_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("adt_or_antiandrogens" = codelist$adt_or_antiandrogens),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = "radical_prostatectomy",
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("adt_or_antiandrogens" = codelist$adt_or_antiandrogens),
    window = c(-Inf, - 181),
    intersections = 0,
    cohortId = "ebrt",
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("other hormones" = codelist$other_hormones),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_end_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Malignancy except non-melanoma skin cancer" = codelist$`Malignancy except non-melanoma`),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("copd" = codelist$copd) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("heart failure" = codelist$heart_failure) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("mi" = codelist$mi) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("stroke" = codelist$stroke) ,
    window = c(-365, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  )|>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Kidney dialysis or transplantation" = codelist$kidney_dialysis_transplantation),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  ) |>
  CohortConstructor::requireConceptIntersect(
    conceptSet = list("Bilateral hip replacement" = codelist$bilateral_hip_replacement),
    window = c(-Inf, 0),
    intersections = 0,
    cohortId = NULL,
    indexDate = "cohort_start_date",
    targetStartDate = "event_start_date",
    targetEndDate = "event_start_date",
  ) |>
  dplyr::filter(!(.data$subject_id %in% excluded_subjects)) |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects with records related to female conditions") |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects both treatment the same day") |>
  dplyr::left_join(cdm$psa_trial |> dplyr::select("subject_id", "psa_value"), by = "subject_id") |>
  dplyr::compute(name = "optima_pc_trial") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt", "radical_prostatectomy"), newCohortName = c("ebrt_trial", "radical_prostatectomy_trial"))

omopgenerics::logMessage("Completed: optima_pc_trial")
# rwd ----
omopgenerics::logMessage("Constructing rwd cohort with eligibility checks")
cdm$optima_pc_rwd <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_rwd") |>

  CohortConstructor::requireIsFirstEntry() |>

  dplyr::group_by(.data$subject_id) |>

  dplyr::slice_min(.data$cohort_start_date) |>

  dplyr::compute(name = "optima_pc_rwd") |>

  CohortConstructor::requirePriorObservation(minPriorObservation = 365) |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("rp_to_exclude" = codelist$rp_exclude),
                                             window = c(-Inf, 0),
                                             intersection = c(0,0),
                                             targetStartDate = "event_start_date",
                                             targetEndDate = "event_start_date",
                                             cohortId = "radical_prostatectomy") |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("Prostate cancer conditions" = codelist$prostate_cancer),
                                             window = c(-Inf, 0),
                                             targetStartDate = "event_start_date",
                                             targetEndDate = "event_start_date",
                                             intersections = c(1, Inf)) |>

  CohortConstructor::requireConceptIntersect(conceptSet = list("pc_to_exclude" = codelist$prostate_cancer_exclude),
                                             targetStartDate = "event_start_date",
                                             targetEndDate = "event_start_date",
                                             window = c(-Inf, 0),
                                             intersection = c(0,0)) |>

  CohortConstructor::requireCohortIntersect(targetCohortTable = "early_stage_rwd",
                                            targetStartDate = "cohort_start_date",
                                            targetEndDate = "cohort_start_date",
                                            window = c(-Inf, 0),
                                            intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(targetCohortTable = "advanced_stage_rwd",
                                            targetStartDate = "cohort_start_date",
                                            targetEndDate = "cohort_start_date",
                                            window = c(-Inf, 0),
                                            intersections = 0) |>
  dplyr::filter(!(.data$subject_id %in% excluded_subjects)) |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects with records related to female conditions") |>
  dplyr::group_by(subject_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup() |>
  omopgenerics::recordCohortAttrition(reason = "Exclude subjects both treatment the same day") |>
  dplyr::compute(name = "optima_pc_rwd") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt", "radical_prostatectomy"),
                                  newCohortName = c("ebrt_rwd", "radical_prostatectomy_rwd"))

omopgenerics::logMessage("Completed: optima_pc_rwd")

omopgenerics::logMessage("Splitting rwd cohort by age: 50–69")

cdm$optima_pc_rwd_50_69 <- cdm$optima_pc_rwd |>
  CohortConstructor::requireAge(ageRange = list(c(50,69)),
                                name = "optima_pc_rwd_50_69" ) |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_rwd", "radical_prostatectomy_rwd"),
                                  newCohortName = c("ebrt_rwd_50_69", "radical_prostatectomy_rwd_50_69"))
omopgenerics::logMessage("Completed: optima_pc_rwd_50_69")

omopgenerics::logMessage("Splitting rwd cohort by age: 70+")
cdm$optima_pc_rwd_70_inf <- cdm$optima_pc_rwd |>
  CohortConstructor::requireAge(ageRange = list(c(70,Inf)),
                                name = "optima_pc_rwd_70_inf" ) |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_rwd", "radical_prostatectomy_rwd"),
                                  newCohortName = c("ebrt_rwd_70_inf", "radical_prostatectomy_rwd_70_inf"))
omopgenerics::logMessage("Completed: optima_pc_rwd_70_inf")

## 2010-2020
omopgenerics::logMessage("Creating calendar-restricted subsets: 2010–2020")
cdm$optima_pc_trial_2010_2020 <- cdm$optima_pc_trial |>
  CohortConstructor::requireInDateRange(dateRange = as.Date(c("2010-01-01","2019-12-31")),
                                        name = "optima_pc_trial_2010_2020") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_trial", "radical_prostatectomy_trial"),
                                  newCohortName = c("ebrt_trial_2010_2020", "radical_prostatectomy_trial_2010_2020"))


cdm$optima_pc_rwd_2010_2020 <- cdm$optima_pc_rwd |>
  CohortConstructor::requireInDateRange(dateRange = as.Date(c("2010-01-01","2019-12-31")),
                                        name = "optima_pc_rwd_2010_2020") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_rwd", "radical_prostatectomy_rwd"),
                                  newCohortName = c("ebrt_rwd_2010_2020", "radical_prostatectomy_rwd_2010_2020"))



cdm$optima_pc_rwd_50_69_2010_2020 <- cdm$optima_pc_rwd_50_69 |>
  CohortConstructor::requireInDateRange(dateRange = as.Date(c("2010-01-01","2019-12-31")),
                                        name = "optima_pc_rwd_50_69_2010_2020") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_rwd_50_69", "radical_prostatectomy_rwd_50_69"),
                                  newCohortName = c("ebrt_rwd_50_69_2010_2020", "radical_prostatectomy_rwd_50_69_2010_2020"))



cdm$optima_pc_rwd_70_inf_2010_2020 <- cdm$optima_pc_rwd_70_inf |>
  CohortConstructor::requireInDateRange(dateRange = as.Date(c("2010-01-01","2019-12-31")),
                                        name = "optima_pc_rwd_70_inf_2010_2020") |>
  CohortConstructor::renameCohort(cohortId = c("ebrt_rwd_70_inf", "radical_prostatectomy_rwd_70_inf"), newCohortName = c("ebrt_rwd_70_inf_2010_2020", "radical_prostatectomy_rwd_70_inf_2010_2020"))


omopgenerics::logMessage("All cohort constructions finished")
