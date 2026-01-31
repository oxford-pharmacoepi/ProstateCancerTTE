
codelist <- importCodelist(here("..", "Codelist", "InclusionCriteria"), type = "csv")
names(codelist)[names(codelist) == "ebrt"] <- "radiotheraphy"
names(codelist)[names(codelist) == "radical_prostatectomy"] <- "prostatectomy"
codelistOutcomes <- importCodelist(here("..", "Codelist", "Outcomes"), type = "csv")
exclude <- importCodelist(path = here("..", "Codelist", "ExcludedFromPS"), type = "csv")

names(codelist) <- toSnakeCase(names(codelist))
names(codelistOutcomes) <- toSnakeCase(names(codelistOutcomes))

# treatments
cdm$treatments <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist[c("prostatectomy", "radiotheraphy")],
  exit = "event_start_date",
  name = "treatments"
) |>
  requireConceptIntersect(
    conceptSet = codelist["rp_exclude"],
    window = c(0, 0),
    intersections = 0,
    cohortId = "radiotheraphy"
  )

# prostate cancer cohort
cdm$prostate_cancer <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist["prostate_cancer"],
  name = "prostate_cancer",
  exit = "event_start_date"
) |>
  requireFutureObservation(minFutureObservation = 1) |>
  requireConceptIntersect(
    conceptSet = codelist["prostate_cancer_exclude"],
    window = c(0, 0),
    intersections = 0
  ) |>
  requireIsFirstEntry() |>
  requireCohortIntersect(
    targetCohortTable = "treatments",
    window = c(-Inf, 0),
    intersections = 0,
    targetCohortId = "radiotheraphy"
  ) |>
  requireCohortIntersect(
    targetCohortTable = "treatments",
    window = c(-Inf, 0),
    intersections = 0,
    targetCohortId = "prostatectomy"
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["adt_or_antiandrogens"],
    window = c(-Inf, -181),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["other_hormones"],
    window = c(-Inf, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["copd"],
    window = c(-365, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["heart_failure"],
    window = c(-365, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["mi"],
    window = c(-365, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["stroke"],
    window = c(-365, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["kidney_dialysis_transplantation"],
    window = c(-Inf, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["bilateral_hip_replacement"],
    window = c(-Inf, 0),
    intersections = 0
  ) |>
  requireConceptIntersect(
    conceptSet = codelist["malignancy_except_non_melanoma"],
    window = c(-Inf, 0),
    intersections = 0
  )

# censoring events
cdm$censor <- conceptCohort(
  cdm = cdm,
  name = "censor",
  conceptSet = codelist[c(
    "bilateral_hip_replacement", "copd", "heart_failure",
    "kidney_dialysis_transplantation", "malignancy_except_non_melanoma", "mi",
    "other_hormones", "stroke"
  )],
  exit = "event_start_date"
) |>
  unionCohorts(cohortName = "censor_event")

# stages
cdm$prostate_cancer <- cdm$prostate_cancer |>
  addConceptIntersectFlag(
    conceptSet = codelist[c("m0", "m1", "t1_t2", "t3_t4", "stage1_2", "stage3_4")],
    window = c(-Inf, 0),
    nameStyle = "{concept_name}"
  ) |>
  mutate(
    early_stage = if_else(stage1_2 == 1 | (m0 == 1 & t1_t2 == 1), 1, 0),
    advanced_stage = if_else(stage3_4 == 1 | (m1 == 1 & t3_t4 == 1), 1, 0),
    stage = case_when(
      early_stage == 1 & advanced_stage == 1 ~ "both",
      early_stage == 1 ~ "early",
      advanced_stage == 1 ~ "adavnced",
      .default = "not recorded"
    )
  ) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, stage) |>
  compute(name = "prostate_cancer")

# psa
cdm[["psa"]] <- cdm$measurement |>
  dplyr::filter(.data$measurement_concept_id %in% codelist$psa) |>
  dplyr::select("person_id" , "measurement_date", "value_as_number") |>
  dplyr::group_by(.data$person_id, .data$measurement_date) |>
  dplyr::summarise(
    min_val         = min(.data$value_as_number, na.rm = TRUE),
    max_val         = max(.data$value_as_number, na.rm = TRUE),
    all_na          = dplyr::if_else(sum(as.integer(!is.na(.data$value_as_number))) == 0L,TRUE, FALSE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    value_as_number = dplyr::case_when(
      all_na ~ NA_real_,
      min_val == max_val ~ min_val,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(.data$value_as_number)) |>
  dplyr::select(
    "subject_id" = "person_id",
    "cohort_start_date" = "measurement_date",
    "cohort_end_date" = "measurement_date",
    "psa_value" = "value_as_number"
  )|>
  dplyr::mutate("cohort_definition_id" = 1L) |>
  PatientProfiles::filterInObservation(indexDate = "cohort_start_date") |>
  dplyr::compute(name = "psa") |>
  omopgenerics::newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "psa_values")) |>
  addCategories(
    variable = "psa_value",
    categories = list("psa_category" = list(
      "[0, 3)" = c(0, 3),
      "[3, 6)" = c(3, 6),
      "[6, 10)" = c(6, 10),
      "[10, 20)" = c(10, 20),
      "[20, 40)" = c(20, 40),
      "[40, Inf)" = c(40, Inf)
    )),
    includeUpperBound = FALSE,
    includeLowerBound = TRUE,
    missingCategoryValue = "Not recorded"
  ) |>
  filter(psa_category != "Not recorded") |>
  compute(name = "psa")

# gleason
cdm[["gleason"]] <- cdm$measurement |>
  dplyr::filter(.data$measurement_concept_id %in% 619648) |>
  dplyr::select("person_id" , "measurement_date", "value_as_number") |>
  dplyr::group_by(.data$person_id, .data$measurement_date) |>
  dplyr::summarise(
    min_val         = min(.data$value_as_number, na.rm = TRUE),
    max_val         = max(.data$value_as_number, na.rm = TRUE),
    all_na          = dplyr::if_else(sum(as.integer(!is.na(.data$value_as_number))) == 0L,TRUE, FALSE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    value_as_number = dplyr::case_when(
      all_na ~ NA_real_,
      min_val == max_val ~ min_val,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(.data$value_as_number)) |>
  dplyr::select("subject_id" = "person_id", "cohort_start_date" = "measurement_date", "cohort_end_date" = "measurement_date",
                "gleason" = "value_as_number"
  )|>
  dplyr::mutate("cohort_definition_id" = 1L) |>
  PatientProfiles::filterInObservation(indexDate = "cohort_start_date") |>
  dplyr::compute(name = "gleason") |>
  omopgenerics::newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "psa_values")) |>
  addCategories(
    variable = "gleason",
    categories = list("gleason_category" = list(
      "<2" = c(0,1),
      "2 to 6" = c(2,6),
      "7" = c(7,7),
      "8 to 10" = c(8,10),
      ">10" = c(11, Inf)
    )),
    includeUpperBound = TRUE,
    includeLowerBound = TRUE,
    missingCategoryValue = "Not recorded"
  ) |>
  filter(gleason_category != "Not recorded") |>
  compute(name = "gleason")

# death cohort
cdm$death_cohort <- deathCohort(cdm = cdm, name = "death_cohort")

# outcomes
cdm$outcomes <- conceptCohort(
  cdm = cdm,
  conceptSet = codelistOutcomes,
  name = "outcomes",
  exit = "event_start_date"
)
