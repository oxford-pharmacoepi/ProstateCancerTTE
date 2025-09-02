
library(readxl)
library(here)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(omopgenerics)
library(CodelistGenerator)
library(stringr)

# inclusion criteria ----
folder_path <- here("Codelist", "CreateCodelists", "SourceCodelists")

codelist_rp_rwd <- codesFromCohort(file.path(folder_path, "pca_rp_rwd.json"), cdm = cdm)
exportCodelist(codelist_rp_rwd, here("Codelist", "pca_rp_rwd"), type = 'csv')

codelist_rt_rwd <- codesFromCohort(file.path(folder_path, "pca_rt_rwd.json"), cdm = cdm)
exportCodelist(codelist_rt_rwd, here("Codelist", "pca_rt_rwd"), type = 'csv')

codelist_rp_trial <- codesFromCohort(file.path(folder_path, "pca_rp_trial.json"), cdm = cdm)
exportCodelist(codelist_rp_trial, here("Codelist", "pca_rp_trial"), type = 'csv')

codelist_rt_trial <- codesFromCohort(file.path(folder_path, "pca_rt_trial.json"), cdm = cdm)
exportCodelist(codelist_rt_trial, here("Codelist", "pca_rt_trial"), type = 'csv')

# outcomes ----
outFile <- here("Codelist", "CreateCodelists", "SourceCodelists", "outcomes_review.xlsx")

## extract
extractCodelist <- function(sheet, cols) {
  codelist <- read_excel(path = outFile, sheet = sheet) |>
    select(all_of(c("concept_id", "concept_name", cols))) |>
    mutate(across(.cols = names(cols), .fns = \(x) dplyr::case_when(
      is.logical(x) ~ as.logical(x),
      x == "F" ~ FALSE,
      x == "FALSE" ~ FALSE,
      x == "T" ~ TRUE,
      x == "TRUE" ~ TRUE
    ))) |>
    pivot_longer(cols = all_of(names(cols))) |>
    filter(value) |>
    group_by(name) |>
    group_split()
  names(codelist) <- map(codelist, \(x) unique(x$name))
  codelist |>
    map(\(x) as.integer(x$concept_id)) |>
    newCodelist()
}

## rectal/bladder/bowel/ureteric injury ----
codelist <- extractCodelist(
  sheet = "outcome1",
  cols = c(injury_broad = "REVIEWED Broad", injury_narrow = "REVIEWED Narrow")
)
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## urethral stricture or incontinence ----
codelist <- extractCodelist(
  sheet = "outcome3",
  cols = c(incontinence_broad = "REV_B", incontinence_narrow = "REV_N")
)
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## erectile dysfunction ----
codelist <- extractCodelist(
  sheet = "outcome4",
  cols = c(erectile_dysfunction_broad = "REV_B", erectile_dysfunction_narrow = "REV_N")
)
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## metastasis ----
codelist <- read_excel(path = outFile, sheet = "outcome5") |>
  select(c("concept_id", "review")) |>
  filter(review == "include") |>
  pull("concept_id") |>
  as.integer() |>
  list() |>
  set_names(nm = "metastasis") |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## cardiovascular disease ----
codelist <- read_excel(path = outFile, sheet = "outcome6") |>
  select(c(name = "cohort_name", "concept_id")) |>
  group_by(name) |>
  group_split()
names(codelist) <- map_chr(codelist, \(x) unique(x$name)) |>
  str_replace_all(pattern = "^cad_", replacement = "")
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## thromboembolic events ----
codelist <- read_excel(path = outFile, sheet = "outcome7") |>
  filter(review == "include") |>
  select(c(name = "cohort_name", "concept_id")) |>
  group_by(name) |>
  group_split()
names(codelist) <- map_chr(codelist, \(x) unique(x$name)) |>
  str_replace_all(pattern = "_v3", replacement = "") |>
  str_replace_all(pattern = "_v4", replacement = "")
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## bone fractures ----
codelist <- read_excel(path = outFile, sheet = "outcome8") |>
  select(c(name = "Site", concept_id = "Id")) |>
  group_by(name) |>
  group_split()
names(codelist) <- paste0("fracture_", map_chr(codelist, \(x) unique(x$name))) |>
  tolower()
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## anxiety and depression ----
codelist <- read_excel(path = outFile, sheet = "outcome9") |>
  pivot_longer(c("anxiety_broad", "anxiety_narrow", "depression_broad", "depression_narrow")) |>
  filter(value) |>
  group_by(name) |>
  group_split()
names(codelist) <- map(codelist, \(x) unique(x$name))
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")

## hypertension ----
codelist <- extractCodelist(
  sheet = "outcome10",
  cols = c(
    hypertension = "ALL SYSTEMIC ARTERIAL HYPERTENSION",
    hypertension_incident = "INCIDENT",
    hypertension_primary = "PRIMARY"
  )
)
exportCodelist(x = codelist, path = here("Codelist", "Outcomes"), type = "csv")
