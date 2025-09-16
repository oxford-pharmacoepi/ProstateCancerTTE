folder_path <- here::here("Codelist")


codelist_ic <- omopgenerics::importCodelist(paste0(folder_path,"/InclusionCriteria"), "csv")

#codelist_outcome <- omopgenerics::importCodelist(paste0(folder_path,"/Outcomes"), "csv")

cdm$pc_cohort <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_ic, name = "pc_cohort")

result <- PhenotypeR::phenotypeDiagnostics(cdm$pc_cohort)

omopgenerics::exportSummarisedResult(result, fileName = "result_codelist_{cdm_name}.csv", path = here::here("Diagnostics/Results"))
