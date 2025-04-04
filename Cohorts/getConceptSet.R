data_path <- file.path(getwd(), "Cohorts/prostate-cohorts")
json_files <- list.files(data_path, pattern = "\\.json$", full.names = TRUE)

conceptSet <- CodelistGenerator::codesFromConceptSet(data_path, cdm = cdm)
