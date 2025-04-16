#cohort rwd

folder_path <- here::here("Diagnostics/Cohorts/codelists/total")


codelist <- omopgenerics::importCodelist(paste0(folder_path), "csv")

codelist_treatment <- list("EBRT" = codelist$`[OPTIMA RQ4] EBRT`, "Radical prostatectomy" = codelist$`[OPTIMA RQ4] Radical prostatectomy`)




cdm$prostate_cancer_first_diagnosis_cohort <- CohortConstructor::conceptCohort(cdm,
                                                               conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
                                                               name = "prostate_cancer_first_diagnosis_cohort" ) |>
  CohortConstructor::requireIsFirstEntry()




cdm$optima_pc_rwd <- CohortConstructor::conceptCohort(cdm, conceptSet = codelist_treatment, name = "optima_pc_rwd") |>
  CohortConstructor::requirePriorObservation(minPriorObservation = 365) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
                                             window = c(-Inf,Inf),
                                             intersections = c(1, Inf)) |>
  CohortConstructor::requireConceptIntersect(conceptSet = list("Prostate cancer conditions" = codelist$`[OPTIMA RQ1] Prostate cancer conditions`),
                                             window = c(-180,180),
                                             intersections = c(1, Inf)) |>
  CohortConstructor::requireCohortIntersect(targetCohortTable = "prostate_cancer_first_diagnosis_cohort",
                                            window = c(-180, 180),
                                            intersections = c(1, Inf))



res <- CohortCharacteristics::summariseCohortAttrition(cdm$optima_pc_rwd)
CohortCharacteristics::plotCohortAttrition(res)
