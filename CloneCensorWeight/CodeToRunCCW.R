library(RPostgres)
library(CDMConnector)
library(omopgenerics, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(here)
library(CohortConstructor)
library(PatientProfiles)
library(glmnet)
library(purrr)
library(nnet)
library(cli)
library(survival)
library(stringr)

logFile <- here("CloneCensorWeight", "Results", "log_{date}_{time}.txt")
createLogFile(logFile = logFile)
source(here("CloneCensorWeight", "functions.R"))

#cdmName <- "CPRD Aurum"
cdmName <- "CPRD GOLD"

con <- dbConnect(drv = Postgres(),
                 dbname = if_else(cdmName == "CPRD GOLD", "cdm_gold_p22_001867", "cdm_aurum_p22_001867"),
                 host = Sys.getenv("DB_HOST"),
                 port = Sys.getenv("DB_PORT"),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

cdm <- cdmFromCon(
  con = con,
  cdmName = cdmName,
  cdmSchema = "public",
  writeSchema = "results",
  writePrefix = "mc_pca_",
  .softValidation = TRUE
)

# filter observation_period
logMessage("Filter observation periods")
cdm$observation_period <- cdm$observation_period |>
  filter(period_type_concept_id == 32882)
cdm <- validateCdmArgument(
  cdm = cdm,
  checkOverlapObservation = TRUE,
  checkStartBeforeEndObservation = TRUE,
  checkPlausibleObservationDates = TRUE,
  checkPerson = TRUE
)

# read codelists
logMessage("Import codelists")
codelist <- importCodelist(here("Codelist", "InclusionCriteria"), type = "csv")
names(codelist)[names(codelist) == "ebrt"] <- "radiotheraphy"
names(codelist)[names(codelist) == "radical_prostatectomy"] <- "prostatectomy"
names(codelist)[names(codelist) == "stage3_4"] <- "progression"

# create cohorts
logMessage("Create prostate cancer cohort")
nm <- "prostate_cancer"
cdm[[nm]] <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist["prostate_cancer"],
  name = nm
) |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireFutureObservation(minFutureObservation = 1) |>
  addDemographics(
    sex = FALSE,
    priorObservation = FALSE,
    name = nm
  ) |>
  addDeathDays(name = nm) |>
  mutate(future_observation = if_else(
    is.na(days_to_death) | days_to_death >= future_observation,
    future_observation,
    days_to_death
  )) |>
  addConceptIntersectDays(
    conceptSet = codelist[c("prostatectomy", "radiotheraphy", "progression")],
    window = c(-Inf, Inf),
    nameStyle = "{concept_name}",
    order = "first",
    name = nm
  ) |>
  filter(is.na(prostatectomy) | prostatectomy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `prostatectomy` before index date") |>
  filter(is.na(radiotheraphy) | radiotheraphy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `radiotheraphy` before index date") |>
  filter(is.na(progression) | progression > 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `progression` before or on index date") |>
  copyCohorts(name = nm, n = 3) |>
  renameCohort(cohortId = "prostate_cancer", newCohortName = "surveillance") |>
  renameCohort(cohortId = "prostate_cancer_1", newCohortName = "prostatectomy") |>
  renameCohort(cohortId = "prostate_cancer_2", newCohortName = "radiotheraphy")
surveillanceId <- getCohortId(cohort = cdm[[nm]], cohortName = "surveillance")
prostatectomyId <- getCohortId(cohort = cdm[[nm]], cohortName = "prostatectomy")
radiotheraphyId <- getCohortId(cohort = cdm[[nm]], cohortName = "radiotheraphy")
cdm[[nm]] <- cdm[[nm]] |>
  addCohortName() |>
  filter(is.na(prostatectomy) | prostatectomy > 0 | cohort_name == "prostatectomy") |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `prostatectomy` on index date", cohortId = c(surveillanceId, radiotheraphyId)) |>
  filter(is.na(radiotheraphy) | radiotheraphy > 0 | cohort_name == "radiotheraphy") |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `radiotheraphy` on index date", cohortId = c(surveillanceId, prostatectomyId))

# basic censoring
cdm[[nm]] <- cdm[[nm]] |>
  mutate(
    prostatectomy = coalesce(prostatectomy, 9999),
    radiotheraphy = coalesce(radiotheraphy, 9999),
    progression = coalesce(progression, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    censor_time = case_when(
      days_to_death <= future_observation & days_to_death <= progression ~ days_to_death,
      progression <= future_observation ~ progression,
      .default = future_observation
    ),
    censor_time = case_when(
      cohort_name == "surveillance" & prostatectomy <= censor_time & prostatectomy <= radiotheraphy ~ prostatectomy,
      cohort_name == "surveillance" & radiotheraphy <= censor_time & radiotheraphy <= prostatectomy ~ radiotheraphy,
      cohort_name == "surveillance" ~ censor_time,
      cohort_name == "prostatectomy" & prostatectomy <= 365 ~ if_else(radiotheraphy <= censor_time, radiotheraphy, censor_time),
      cohort_name == "prostatectomy" ~ if_else(365 <= censor_time, 365, censor_time),
      cohort_name == "radiotheraphy" & radiotheraphy <= 365 ~ if_else(prostatectomy <= censor_time, prostatectomy, censor_time),
      cohort_name == "radiotheraphy" ~ if_else(365 <= censor_time, 365, censor_time)
    ),
    censor_reason = case_when(
      censor_time == prostatectomy & cohort_name != "prostatectomy" ~ "prostatectomy",
      censor_time == radiotheraphy & cohort_name != "radiotheraphy" ~ "radiotheraphy",
      censor_time == progression ~ "progression",
      censor_time == days_to_death ~ "death",
      censor_time == future_observation ~ "end observation",
      censor_time == 365 ~ "no treatment",
      .default = "error"
    )
  ) |>
  compute(name = nm)

logMessage("Extract covariates")
min_frequency <- 0.005
num_subjects <- cdm[[nm]] |>
  distinct(subject_id) |>
  tally() |>
  pull() |>
  as.numeric()
min_subjects <- num_subjects * min_frequency

# eclude concepts from propensity scores
exclude <- importCodelist(path = here("Codelist", "ExcludedFromPS"), type = "csv")
conceptsToExclude <- c(
  codelist$radiotheraphy, codelist$prostate_cancer, codelist$prostatectomy,
  unlist(exclude, use.names = FALSE)
)

# extract conditions
logMessage("Extract conditions")
conditions <- cdm$condition_occurrence |>
  select(subject_id = "person_id", "condition_start_date", concept_id = "condition_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      group_by(subject_id) |>
      summarise(
        cohort_start_date = min(cohort_start_date, na.rm = TRUE),
        censor_time = max(censor_time, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "subject_id"
  ) |>
  mutate(cov_time = condition_start_date - cohort_start_date) |>
  filter(cov_time < censor_time) |>
  distinct(subject_id, cov_time, concept_id) |>
  collect()
conds <- conditions |>
  group_by(concept_id) |>
  summarise(n_subjects = n_distinct(subject_id)) |>
  filter(concept_id != 0) |>
  filter(n_subjects >= min_subjects) |>
  filter(!concept_id %in% conceptsToExclude) |>
  pull(concept_id)
conditions <- conditions |>
  filter(concept_id %in% conds)

# extract exposures
logMessage("Extract exposures")
exposures <- cdm$drug_exposure |>
  select(subject_id = "person_id", "drug_exposure_start_date", concept_id = "drug_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      group_by(subject_id) |>
      summarise(
        cohort_start_date = min(cohort_start_date, na.rm = TRUE),
        censor_time = max(censor_time, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "subject_id"
  ) |>
  mutate(cov_time = drug_exposure_start_date - cohort_start_date) |>
  filter(cov_time < censor_time & cov_time >= -365) |>
  distinct(subject_id, cov_time, concept_id) |>
  collect()
exps <- exposures |>
  group_by(concept_id) |>
  summarise(n_subjects = n_distinct(subject_id)) |>
  filter(concept_id != 0) |>
  filter(n_subjects >= min_subjects) |>
  filter(!concept_id %in% conceptsToExclude) |>
  pull(concept_id)
exposures <- exposures |>
  filter(concept_id %in% exps)

# collect data
logMessage("Collect data")
x <- cdm[[nm]] |>
  select("cohort_name", "subject_id", "age", death = "days_to_death", "censor_time", "censor_reason") |>
  collect() |>
  mutate(cohort_name = factor(cohort_name, levels = c("surveillance", "prostatectomy", "radiotheraphy")))

result <- summaryOutcome(x = x, outcome = "death", conditions = conditions, exposures = exposures, min_frequency = min_frequency)
