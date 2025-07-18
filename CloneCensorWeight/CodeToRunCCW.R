library(RPostgres)
library(CDMConnector)
library(omopgenerics)
library(here)
library(CohortConstructor)
library(PatientProfiles)

con <- dbConnect(drv = Postgres(),
                 dbname = "cdm_gold_202501",
                 #dbname = "cdm_gold_202501",
                 host = Sys.getenv("DB_HOST"),
                 port = Sys.getenv("DB_PORT"),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

cdm <- cdmFromCon(
  con = con,
  cdmName = "CPRD GOLD",
  #cdmName = "CPRD Aurum",
  cdmSchema = "public",
  writeSchema = "results",
  writePrefix = "mc_pca_"
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
    window = c(Inf, Inf),
    nameStyle = "{cohort_name}",
    order = "first",
    name = nm
  ) |>
  filter(!is.na(prostatectomy) | prostatectomy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `prostatectomy` before index date") |>
  filter(!is.na(radiotheraphy) | radiotheraphy >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `radiotheraphy` before index date") |>
  filter(!is.na(progression) | progression >= 0) |>
  compute(name = nm) |>
  recordCohortAttrition(reason = "no prior `progression` before index date") |>
  copyCohorts(name = nm, n = 3) |>
  renameCohort(cohortId = "prostate_cancer", newCohortName = "surveillance") |>
  renameCohort(cohortId = "prostate_cancer_1", newCohortName = "prostatectomy") |>
  renameCohort(cohortId = "prostate_cancer_2", newCohortName = "radiotheraphy")

conditions <- cdm$condition_occurrence |>
  select(subject_id = "person_id", "condition_start_date", concept_id = "condition_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date", "future_observation"),
    by = "subject_id"
  ) |>
  mutate(time = datediff(cohort_start_date, condition_start_date)) |>
  filter(time < future_observation) |>
  select("subject_id", "time", "concept_id") |>
  collect()

exposures <- cdm$drug_exposure |>
  select(subject_id = "person_id", "drug_exposure_start_date", concept_id = "drug_concept_id") |>
  inner_join(
    cdm[[nm]] |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date", "future_observation"),
    by = "subject_id"
  ) |>
  mutate(time = datediff(cohort_start_date, drug_exposure_start_date)) |>
  filter(time < future_observation & time >= -365) |>
  select("subject_id", "time", "concept_id") |>
  collect()
