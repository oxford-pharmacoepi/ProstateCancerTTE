
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
library(readr)
library(OmopSketch)
library(CodelistGenerator)
library(CohortCharacteristics)

# create log file
logFile <- here("Results", "log_{date}_{time}.txt")
createLogFile(logFile = logFile)

# source functions
source(here("functions.R"))

# create cdm object
logMessage("Create cdm object")
cdmName <- "CPRD Aurum"
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

# snapshot
snapshot <- summariseOmopSnapshot(cdm = cdm)

# read codelists
logMessage("Import codelists")
codelist <- importCodelist(here("Codelist", "InclusionCriteria"), type = "csv")
names(codelist)[names(codelist) == "ebrt"] <- "radiotheraphy"
names(codelist)[names(codelist) == "radical_prostatectomy"] <- "prostatectomy"
names(codelist)[names(codelist) == "stage3_4"] <- "progression"
codelistConditions <- importCodelist(here("Codelist", "conditions"), type = "csv")
codelistMedications <- importCodelist(here("Codelist", "medications"), type = "csv")
codelistOutcomes <- importCodelist(here("Codelist", "Outcomes"), type = "csv")
exclude <- importCodelist(path = here("Codelist", "ExcludedFromPS"), type = "csv")

names(codelistOutcomes) <- toSnakeCase(names(codelistOutcomes))

codelistOfInterest <- list(
  index = codelist[c("radiotheraphy", "prostatectomy", "prostate_cancer")],
  characterisation_conditions = codelistConditions,
  characterisation_medications = codelistMedications,
  outcome = c(codelistOutcomes, codelist["progression"]),
  exclude = exclude
)

# export codelists
logMessage("Document and export codelists")
exportCodelists <- codelistOfInterest |>
  map(\(codes) {
    map(codes, \(x) tibble(concept_id = as.integer(x))) |>
      bind_rows(.id = "codelist_name")
  }) |>
  bind_rows(.id = "codelist_type") |>
  relocate("codelist_type", "codelist_name")
nm <- uniqueTableName()
cdm <- insertTable(cdm = cdm, name = nm, table = exportCodelists)
exportCodelists <- cdm[[nm]] |>
  inner_join(
    cdm$concept |>
      select("concept_name", "concept_id", "domain_id", "vocabulary_id", "concept_class_id", "concept_code"),
    by = "concept_id"
  ) |>
  collect()
write_csv(x = exportCodelists, file = here("CloneCensorWeight", "Results", "codelists.csv"))

# code use
logMessage("Code use")
codeUse <- summariseCodeUse(x = flatten(codelistOfInterest), cdm = cdm)

# create cohorts
logMessage("Create index cohorts")
cdm$index_cohort <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist["prostate_cancer"],
  exit = "event_start_date",
  name = "index_cohort"
) |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireFutureObservation(minFutureObservation = 1) |>
  addDemographics(
    sex = FALSE,
    priorObservation = FALSE,
    name = "index_cohort"
  ) |>
  addDeathDays(name = "index_cohort") |>
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
    name = "index_cohort"
  ) |>
  filter(is.na(prostatectomy) | prostatectomy >= 0) |>
  compute(name = "index_cohort") |>
  recordCohortAttrition(reason = "no prior `prostatectomy` before index date") |>
  filter(is.na(radiotheraphy) | radiotheraphy >= 0) |>
  compute(name = "index_cohort") |>
  recordCohortAttrition(reason = "no prior `radiotheraphy` before index date") |>
  filter(is.na(progression) | progression > 0) |>
  compute(name = "index_cohort") |>
  recordCohortAttrition(reason = "no prior `progression` before or on index date") |>
  copyCohorts(name = "index_cohort", n = 3) |>
  renameCohort(cohortId = "prostate_cancer", newCohortName = "surveillance") |>
  renameCohort(cohortId = "prostate_cancer_1", newCohortName = "prostatectomy") |>
  renameCohort(cohortId = "prostate_cancer_2", newCohortName = "radiotheraphy")
surveillanceId <- getCohortId(cohort = cdm$index_cohort, cohortName = "surveillance")
prostatectomyId <- getCohortId(cohort = cdm$index_cohort, cohortName = "prostatectomy")
radiotheraphyId <- getCohortId(cohort = cdm$index_cohort, cohortName = "radiotheraphy")
cdm$index_cohort <- cdm$index_cohort |>
  addCohortName() |>
  filter(is.na(prostatectomy) | prostatectomy > 0 | cohort_name == "prostatectomy") |>
  compute(name = "index_cohort") |>
  recordCohortAttrition(reason = "no `prostatectomy` on index date", cohortId = c(surveillanceId, radiotheraphyId)) |>
  filter(is.na(radiotheraphy) | radiotheraphy > 0 | cohort_name == "radiotheraphy") |>
  compute(name = "index_cohort") |>
  recordCohortAttrition(reason = "no `radiotheraphy` on index date", cohortId = c(surveillanceId, prostatectomyId))

# cohorts of interest
logMessage("Create cohorts of interest")
cdm$cohort_of_interest <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist[c("prostate_cancer", "prostatectomy", "radiotheraphy")],
  exit = "event_start_date",
  name = "cohort_of_interest"
) |>
  copyCohorts(name = "cohort_of_interest", n = 2) |>
  renameCohort(cohortId = "prostate_cancer", newCohortName = "any_prostate_cancer") |>
  renameCohort(cohortId = "prostatectomy", newCohortName = "any_prostatectomy") |>
  renameCohort(cohortId = "radiotheraphy", newCohortName = "any_radiotheraphy") |>
  renameCohort(cohortId = "prostate_cancer_1", newCohortName = "first_prostate_cancer") |>
  renameCohort(cohortId = "prostatectomy_1", newCohortName = "first_prostatectomy") |>
  renameCohort(cohortId = "radiotheraphy_1", newCohortName = "first_radiotheraphy") |>
  requireIsFirstEntry(cohortId = c("first_prostate_cancer", "first_prostatectomy", "first_radiotheraphy"))
cdm <- bind(
  cdm$cohort_of_interest,
  cdm$index_cohort |>
    select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"),
  name = "cohort_of_interest"
)

# summarise cohort count
logMessage("Summarise count")
summaryCount <- summariseCohortCount(cohort = cdm$cohort_of_interest)

# summarise cohort attrition
logMessage("Summarise attrition")
summaryAttrition <- summariseCohortAttrition(cohort = cdm$cohort_of_interest)

# summarise cohort timing
logMessage("Summarise timing")
summaryTiming <- summariseCohortTiming(cohort = cdm$cohort_of_interest)

# summarise cohort overlap
logMessage("Summarise overlap")
summaryOverlap <- summariseCohortOverlap(cohort = cdm$cohort_of_interest)

# summarise characteristics
logMessage("Summarise characteristics")
summaryCharacteristics <- cdm$cohort_of_interest |>
  left_join(
    cdm$person |>
      select("subject_id" = "person_id", "year_of_birth"),
    by = "subject_id"
  ) |>
  compute(name = "cohort_of_interest") |>
  addConceptIntersectFlag(
    conceptSet = list(primary_care = 581477, hospital = 9201, registry = 38004268),
    window = c(0, 0),
    nameStyle = "{concept_name}",
    name = "cohort_of_interest"
  ) |>
  mutate(diagnostic = case_when(
    primary_care == 1 & hospital == 1 & registry == 1 ~ "primary_care, registry and hospital",
    primary_care == 1 & hospital == 1 ~ "primary_care and hospital",
    primary_care == 1 & registry == 1 ~ "primary_care and registry",
    hospital == 1 & registry == 1 ~ "hospital and registry",
    primary_care == 1 ~ "primary_care",
    hospital == 1 ~ "hospital",
    registry == 1 ~ "registry",
    .default = "none"
  )) |>
  select(!c("primary_care", "hospital", "registry")) |>
  compute(name = "cohort_of_interest") |>
  summariseCharacteristics(
    ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)),
    counts = TRUE,
    demographics = TRUE,
    conceptIntersectFlag = list(
      "Conditions any time prior" = list(
        conceptSet = codelistConditions, window = c(-Inf, 0)
      ),
      "Medications year prior" = list(
        conceptSet = codelistMedications, window = c(-365, 0)
      )
    ),
    conceptIntersectCount = list(
      "Visit counts in the year prior" = list(
        conceptSet = list(primary_care = 581477, hospital = 9201, registry = 38004268),
        window = c(-365, 0)
      )
    ),
    otherVariables = c("diagnostic", "year_of_birth"),
    estimates = list(
      cohort_start_date = "density",
      age = "density",
      prior_observation = "density",
      future_observation = "density",
      diagnostic = c("count", "percentage"),
      year_of_birth = "density"
    )
  )

# summarise large scale characteristics
logMessage("Summarise lsc")
summaryLsc <- summariseLargeScaleCharacteristics(
  cohort = cdm$cohort_of_interest,
  eventInWindow = c("observation", "condition_occurrence", "procedure_occurrence"),
  episodeInWindow = "drug_exposure",
  window = list(c(-Inf, -366), c(-365, -1), c(0, 0), c(1, 365), c(366, Inf))
)

# basic censoring
logMessage("Add censoring to index cohorts")
cdm$index_cohort <- cdm$index_cohort |>
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
  compute(name = "index_cohort")

# eclude concepts from propensity scores
logMessage("Extract covariates")
conceptsToExclude <- c(
  codelist$radiotheraphy, codelist$prostate_cancer, codelist$prostatectomy,
  unlist(exclude, use.names = FALSE)
)

# extract conditions
logMessage("Extract conditions")
conditions <- cdm$condition_occurrence |>
  select(subject_id = "person_id", "condition_start_date", concept_id = "condition_concept_id") |>
  inner_join(
    cdm$index_cohort |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date"),
    by = "subject_id"
  ) |>
  filter(condition_start_date <= cohort_start_date) |>
  distinct(subject_id, concept_id) |>
  collect() |>
  filter(concept_id != 0) |>
  filter(!concept_id %in% conceptsToExclude)

# extract exposures
logMessage("Extract exposures")
exposures <- cdm$drug_exposure |>
  select(subject_id = "person_id", "drug_exposure_start_date", concept_id = "drug_concept_id") |>
  inner_join(
    cdm$index_cohort |>
      filter(cohort_definition_id == 1) |>
      select("subject_id", "cohort_start_date"),
    by = "subject_id"
  ) |>
  mutate(cov_time = drug_exposure_start_date - cohort_start_date) |>
  filter(cov_time <= 0 & cov_time >= -365) |>
  distinct(subject_id, concept_id) |>
  collect() |>
  filter(concept_id != 0) |>
  filter(!concept_id %in% conceptsToExclude)

# add ouctomes information
logMessage("Add outcomes information")
outcomes <- names(codelistOutcomes)
cdm$index_cohort <- cdm$index_cohort |>
  addConceptIntersectDays(
    conceptSet = codelistOutcomes,
    window = c(0, Inf),
    nameStyle = "{concept_name}",
    name = "index_cohort"
  ) |>
  mutate(across(.cols = all_of(outcomes), .fns = \(x) coalesce(x, 9999))) |>
  compute(name = "index_cohort")

# collect data
logMessage("Collect data")
x <- cdm$index_cohort |>
  select("cohort_name", "subject_id", "age", death = "days_to_death", "progression", "censor_time", "censor_reason", all_of(outcomes)) |>
  collect() |>
  mutate(cohort_name = factor(cohort_name, levels = c("surveillance", "prostatectomy", "radiotheraphy")))

# calculate weights
logMessage("Calculate weights")
weights <- calculateWeights(x, conditions, exposures, 0.005)

# loop through outcomes
outcomes <- c("death", "progression", outcomes)
result <- outcomes |>
  map(\(out) summaryOutcome(x = x, outcome = out, weights = weights)) |>
  # bind and convert results
  bindResults(cdmName = cdmName)

# export results
exportSummarisedResult(
  snapshot,
  codeUse,
  summaryCount,
  summaryAttrition,
  summaryTiming,
  summaryOverlap,
  summaryCharacteristics,
  summaryLsc,
  result,
  path = here("CloneCensorWeight", "Results"),
  fileName = "results_{cdm_name}.csv"
)

# drop created tables
dropSourceTable(cdm = cdm, name = everything())
