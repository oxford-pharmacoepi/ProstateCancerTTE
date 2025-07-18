folder_path <- here::here("CodelistDiagnostics/Codelist/codelists")


codelists_rp_rwd <- omopgenerics::importCodelist(paste0(folder_path,"/rp_rwd"), "csv")

codelists_rt_rwd <- omopgenerics::importCodelist(paste0(folder_path,"/rt_rwd"), "csv")


codelists_rp_trial <- omopgenerics::importCodelist(paste0(folder_path,"/rp_trial"), "csv")


codelists_rt_trial <- omopgenerics::importCodelist(paste0(folder_path,"/rt_trial"), "csv")


codelist <- omopgenerics::newCodelist(c(codelists_rp_rwd, codelists_rt_rwd, codelists_rp_trial, codelists_rt_trial))

codelist$`[OPTIMA PCa RQ4] cT1-T2_rwd` <- codelist$`[OPTIMA PCa RQ4] cT1-T2_1`
codelist$`[OPTIMA PCa RQ4] cT1-T2_trial` <- codelist$`[OPTIMA PCa RQ4] cT1-T2_2`

codelist$`[OPTIMA PCa RQ4] cT1-T2_1` <- NULL
codelist$`[OPTIMA PCa RQ4] cT1-T2_2` <- NULL

codelist$`[OPTIMA PCa RQ4] M0_rwd` <- codelist$`[OPTIMA PCa RQ4] M0_1`
codelist$`[OPTIMA PCa RQ4] M0_trial` <- codelist$`[OPTIMA PCa RQ4] M0_2`

codelist$`[OPTIMA PCa RQ4] M0_1` <- NULL
codelist$`[OPTIMA PCa RQ4] M0_2` <- NULL


cdm$pc_cohort <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist, name = "pc_cohort")

result <- PhenotypeR::phenotypeDiagnostics(cdm$pc_cohort)

omopgenerics::exportSummarisedResult(result_codelist, fileName = "result_codelist_{cdm_name}.csv", path = here::here("Results/CodelistDiagnostics"))

codelists_rp_rwd$`[OPTIMA PCa RQ4] cT1-T2` <- codelists_rp_trial$`[OPTIMA PCa RQ4] cT1-T2`
codelists_rp_rwd$`[OPTIMA PCa RQ4] M0` <-codelists_rp_trial$`[OPTIMA PCa RQ4] M0`
codelists_rt_rwd$`[OPTIMA PCa RQ4] cT1-T2` <- codelists_rt_trial$`[OPTIMA PCa RQ4] cT1-T2`
codelists_rt_rwd$`[OPTIMA PCa RQ4] M0` <-codelists_rt_trial$`[OPTIMA PCa RQ4] M0`


codelists_rp_rwd |> omopgenerics::exportCodelist(path = paste0(folder_path,"/rp_rwd") , type = "csv")

codelists_rt_rwd |> omopgenerics::exportCodelist(path = paste0(folder_path,"/rt_rwd"), type = "csv")

omopgenerics::exportCodelist(c(codelists_rp_rwd, codelists_rt_rwd, codelists_rp_trial, codelists_rt_trial), type = "csv", path = here::here("Cohorts/ConceptSets"))
