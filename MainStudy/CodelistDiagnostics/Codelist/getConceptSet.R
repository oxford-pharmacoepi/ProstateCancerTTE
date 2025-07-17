folder_path <- here::here("CodelistDiagnostics/Codelist/prostate-cohorts-atlas/")

codelist_rp_rwd <- CodelistGenerator::codesFromCohort(paste0(folder_path, "pca_rp_rwd.json"), cdm = cdm)

codelist_rt_rwd <- CodelistGenerator::codesFromCohort(paste0(folder_path, "pca_rt_rwd.json"), cdm = cdm)

codelist_rp_trial <- CodelistGenerator::codesFromCohort(paste0(folder_path, "pca_rp_trial.json"), cdm = cdm)

codelist_rt_trial <- CodelistGenerator::codesFromCohort(paste0(folder_path, "pca_rt_trial.json"), cdm = cdm)


omopgenerics::exportCodelist(codelist_rp_rwd, here::here("CodelistDiagnostics/Codelist/codelists/rp_rwd"), type = 'csv')

omopgenerics::exportCodelist(codelist_rt_rwd, here::here("CodelistDiagnostics/Codelist/codelists/rt_rwd"), type = 'csv')

omopgenerics::exportCodelist(codelist_rp_trial, here::here("CodelistDiagnostics/Codelist/codelists/rp_trial"), type = 'csv')

omopgenerics::exportCodelist(codelist_rt_trial, here::here("CodelistDiagnostics/Codelist/codelists/rt_trial"), type = 'csv')

omopgenerics::exportCodelist(codelist_rp_rwd, path,here::here("CodelistDiagnostics/Codelist/codelists/rp_rwd"), type = 'csv')
