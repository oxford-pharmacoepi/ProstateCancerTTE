
library(readxl)
library(here)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(omopgenerics)
library(CodelistGenerator)

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
## anxiety and depression ----
codelist <- read_excel(here("Codelists", "CreateCodelists", "SourceCodelists", "edirect_codelists_v2.xlsx")) |>
  pivot_longer(c("anxiety_broad", "anxiety_narrow", "depression_broad", "depression_narrow")) |>
  filter(value) |>
  group_by(name) |>
  group_split()
names(codelist) <- map(codelist, \(x) unique(x$name))
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = here("Codelists", "Outcomes"), type = "csv")
