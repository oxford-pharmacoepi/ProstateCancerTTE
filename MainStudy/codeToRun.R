renv::restore()
dbName <- "..."

con <- DBI::dbConnect("...")

cdmSchema <- "..."
writeSchema <- "..."

prefix <- "..."

minCellCount <- 5


createCohorts <- TRUE
runModel <- TRUE
cohorts <- c("optima_pc_trial", "optima_pc_rwd", "optima_pc_rwd_50_69", "optima_pc_rwd_70_inf", "optima_pc_trial_2010_2020", "optima_pc_rwd_2010_2020", "optima_pc_rwd_50_69_2010_2020", "optima_pc_rwd_70_inf_2010_2020")


cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema,
                                writeSchema =  writeSchema,
                                writePrexix = prefix,
                                cdmName = dbName,
                                cohortTables = c(cohorts, "psa_values_rwd", "psa_values_trial","prostate_cancer_age_50_69","n_status_rwd", "n_status_trial",
                                                 "t_status_rwd", "t_status_trial", "gleason_rwd", "gleason_trial", "conditions", "medications", "type2_diabetes" )[!createCohorts]
                                )

# cdm$observation_period<- cdm$observation_period |>
#   dplyr::filter(.data$period_type_concept_id == 32882) uncomment if running in CPRD

source("RunAnalysis.R")


CDMConnector::cdmDisconnect(cdm)
