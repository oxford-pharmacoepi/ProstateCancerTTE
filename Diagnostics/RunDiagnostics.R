
folder_path <- here::here("..", "Codelist")
output_folder <- here::here("Results")

result <- list()

log_file <- file.path(output_folder, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))

omopgenerics::createLogFile(logFile = log_file)

omopgenerics::logMessage("Running PhenotypeR diagnostics for inclusion criteria")

codelist_ic <- omopgenerics::importCodelist(paste0(folder_path,"/InclusionCriteria"), "csv")

cdm$inclusion_criteria_cohort <- CohortConstructor::conceptCohort(cdm, 
                                                                  conceptSet = codelist_ic, 
                                                                  table = c("condition_occurrence", "drug_exposure", "procedure_occurrence", "measurement", "observation", "device_exposure"), 
                                                                  name = "inclusion_criteria_cohort")
cdm$inclusion_criteria_cohort2 <- CohortConstructor::conceptCohort(cdm, 
                                                                  conceptSet = codelist_ic, 
                                                                  name = "inclusion_criteria_cohort2")

if (fullDiagnostics){
  result[["inclusion_criteria"]] <- PhenotypeR::phenotypeDiagnostics(cdm$inclusion_criteria_cohort)
} else {
  result[["inclusion_criteria"]] <- PhenotypeR::codelistDiagnostics(cdm$inclusion_criteria_cohort)
}
omopgenerics::logMessage("Running codelist diagnostics for outcomes")

codelist_outcome <- omopgenerics::importCodelist(paste0(folder_path,"/Outcomes"), "csv")

cdm$outcome_cohort <- CohortConstructor::conceptCohort(cdm, 
                                                       conceptSet = codelist_outcome, 
                                                       table = c("condition_occurrence", "drug_exposure", "procedure_occurrence", "measurement", "observation", "device_exposure"), 
                                                       name = "outcome_cohort")

result[["outcome"]] <- PhenotypeR::codelistDiagnostics(cdm$outcome_cohort)

omopgenerics::logMessage("Exporting results")

result <- result |> omopgenerics::bind()

omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, fileName = "result_diagnostics_{cdm_name}_{date}.csv", path = output_folder)
