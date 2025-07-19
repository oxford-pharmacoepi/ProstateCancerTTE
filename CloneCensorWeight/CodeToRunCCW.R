library(RPostgres)
library(CDMConnector)
library(omopgenerics, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(here)
library(CohortConstructor)
library(PatientProfiles)
library(CohortCharac)

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
codelist <- importCodelist(here("Codelist", "InclusionCriteria"), type = "csv")
names(codelist)[names(codelist) == "ebrt"] <- "radiotheraphy"
names(codelist)[names(codelist) == "radical_prostatectomy"] <- "prostatectomy"
names(codelist)[names(codelist) == "stage3_4"] <- "progression"

# create cohorts
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
  renameCohort(cohortId = "prostate_cancer_2", newCohortName = "radiotheraphy") |>
  filter(is.na(prostatectomy) | prostatectomy > 0 | cohort_definition_id == 2) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `prostatectomy` on index date", cohortId = c(1, 3)) |>
  filter(is.na(radiotheraphy) | radiotheraphy > 0 | cohort_definition_id == 3) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no `radiotheraphy` on index date", cohortId = c(1, 2))

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
      cohort_definition_id == 1 & prostatectomy <= censor_time & prostatectomy <= radiotheraphy ~ prostatectomy,
      cohort_definition_id == 1 & radiotheraphy <= censor_time & radiotheraphy <= prostatectomy ~ radiotheraphy,
      cohort_definition_id == 1 ~ censor_time,
      cohort_definition_id == 2 & prostatectomy <= 365 ~ if_else(radiotheraphy <= censor_time, radiotheraphy, censor_time),
      cohort_definition_id == 2 ~ if_else(365 <= censor_time, 365, censor_time),
      cohort_definition_id == 3 & radiotheraphy <= 365 ~ if_else(prostatectomy <= censor_time, prostatectomy, censor_time),
      cohort_definition_id == 3 ~ if_else(365 <= censor_time, 365, censor_time)
    ),
    censor_reason = case_when(
      censor_time == prostatectomy & cohort_definition_id != 2 ~ "prostatectomy",
      censor_time == radiotheraphy & cohort_definition_id != 3 ~ "radiotheraphy",
      censor_time == progression ~ "progression",
      censor_time == days_to_death ~ "death",
      censor_time == future_observation ~ "end observation",
      censor_time == 365 ~ "no treatment",
      .default = "error"
    )
  ) |>
  compute(name = nm)

# extract conditions
conditions <- cdm$condition_occurrence |>
  select(subject_id = "person_id", "condition_start_date", concept_id = "condition_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date", "censor_time"),
    by = "subject_id"
  ) |>
  mutate(time = condition_start_date - cohort_start_date) |>
  filter(time < censor_time) |>
  select("subject_id", "time", "concept_id") |>
  collect()

# extract exposures
exposures <- cdm$drug_exposure |>
  select(subject_id = "person_id", "drug_exposure_start_date", concept_id = "drug_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date", "censor_time"),
    by = "subject_id"
  ) |>
  mutate(time = drug_exposure_start_date - cohort_start_date) |>
  filter(time < censor_time & time >= -365) |>
  select("subject_id", "time", "concept_id") |>
  collect()

# collect data
x <- cdm[[nm]] |>
  select("cohort_definition_id", "subject_id", death = "days_to_death", "censor_time", "censor_reason") |>
  collect()

outcome <- "death"
x <- x |>
  rename(outcome = all_of(outcome)) |>
  mutate(
    status = if_else(is.na(outcome) | outcome > censor_time, 0, 1),
    time = if_else(status == 0, censor_time, outcome),
    reason = if_else(status == 0, censor_reason, "outcome")
  ) |>
  select("cohort_definition_id", "subject_id", "status", "time", "reason")

time <-
