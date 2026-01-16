
library(readxl)
library(here)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(omopgenerics)
library(CodelistGenerator)
library(stringr)

# inclusion criteria ----
folder_path <- "~/ProstateCancerTTE/Codelist/CreateCodelists/SourceCodelists"
# 
# 
# codelist_rp_rwd <- codesFromCohort(file.path(folder_path, "pca_rp_rwd.json"), cdm = cdm)
# exportCodelist(codelist_rp_rwd, here("Codelist", "pca_rp_rwd"), type = 'csv')
# 
# codelist_rt_rwd <- codesFromCohort(file.path(folder_path, "pca_rt_rwd.json"), cdm = cdm)
# exportCodelist(codelist_rt_rwd, here("Codelist", "pca_rt_rwd"), type = 'csv')
# 
# codelist_rp_trial <- codesFromCohort(file.path(folder_path, "pca_rp_trial.json"), cdm = cdm)
# exportCodelist(codelist_rp_trial, here("Codelist", "pca_rp_trial"), type = 'csv')
# 
# codelist_rt_trial <- codesFromCohort(file.path(folder_path, "pca_rt_trial.json"), cdm = cdm)
# exportCodelist(codelist_rt_trial, here("Codelist", "pca_rt_trial"), type = 'csv')
# 

# diabetes ----
file_path <- paste0(folder_path,"/diabetes.xlsx")
sheets <- excel_sheets(file_path)
out_path <- "~/ProstateCancerTTE/Codelist/Diabetes"
for(sheet in sheets){
  x <- list(read_excel(path = file_path, sheet = sheet)$concept_id)
  names(x) <- sheet
  
  exportCodelist(newCodelist(x), path = out_path , type = "csv")

}


# outcomes ----
file_path <- paste0(folder_path,"/outcomes_final.xlsx")
out_path <- "~/ProstateCancerTTE/Codelist/Outcomes"
## extract
extractCodelist <- function(sheet, cols) {
  codelist <- read_excel(path = file_path, sheet = sheet) |>
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
  cols = c(injury_broad = "broad", injury_narrow = "narrow")
)
exportCodelist(x = codelist, path = out_path, type = "csv")

## initiation of androgen deprivation therapy  ----

codelist <- read_excel(file_path, sheet = "outcome2") |>
  pull("concept_id") |>
  as.integer() |>
  list() |>
  set_names(nm = "androgen_deprivation") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")


## urethral stricture or incontinence ----
codelist <- extractCodelist(
  sheet = "outcome3",
  cols = c(incontinence_broad = "broad", incontinence_narrow = "narrow")
)
exportCodelist(x = codelist, path = out_path, type = "csv")

## erectile dysfunction ----

codelist <- extractCodelist(
  sheet = "outcome4",
  cols = c(erectile_dysfunction_broad = "broad", erectile_dysfunction_narrow = "narrow")
)
exportCodelist(x = codelist, path = out_path, type = "csv")

## metastasis ----
codelist <- read_excel(path = file_path, sheet = "outcome5") |>
  pull("concept_id") |>
  as.integer() |>
  list() |>
  set_names(nm = "metastasis") |>
  newCodelist()
exportCodelist(x = codelist, path = out_path, type = "csv")

## cardiovascular disease ----

codelist <- read_excel(path = file_path, sheet = "outcome6") |>
  select(c(name = "cohort_name", "concept_id")) |>
  group_by(name) |>
  group_split()
names(codelist) <- map_chr(codelist, \(x) unique(x$name)) |>
  str_replace_all(pattern = "^cad_", replacement = "")
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = out_path, type = "csv")

## thromboembolic events ----
codelist <- read_excel(path = file_path, sheet = "outcome7") |>
  select(c(name = "cohort_name", "concept_id")) |>
  group_by(name) |>
  group_split()
names(codelist) <- map_chr(codelist, \(x) unique(x$name)) |>
  str_replace_all(pattern = "_v3", replacement = "") |>
  str_replace_all(pattern = "_v4", replacement = "")
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = out_path, type = "csv")


## any bone fracture ----

codelist <- read_excel(file_path, sheet = "outcome8") |>
  pull("concept_id") |>
  as.integer() |>
  list() |>
  set_names(nm = "any_fracture") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")

## osteoporotic fractures ----
codelist <- read_excel(path = file_path, sheet = "outcome8") |>
  filter(.data$Site %in% c("Forearm", "Vertebra", "Hip", "Humerus", "Osteoporotic")) |>
  pull(concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "osteoporotic_fractures") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")

## anxiety and depression ----
codelist <- read_excel(path = file_path, sheet = "outcome9") |>
  pivot_longer(c("anxiety_broad", "anxiety_narrow", "depression_broad", "depression_narrow")) |>
  mutate(value = as.logical(.data$value)) |>
  filter(.data$value) |>
  group_by(name) |>
  group_split()
names(codelist) <- map(codelist, \(x) unique(x$name))
codelist <- codelist |>
  map(\(x) as.integer(x$concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = out_path, type = "csv")

## hypertension ----

codelist <- read_excel(path = file_path, sheet = "outcome10") |>
  pull(concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "hypertension") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")

## hypercholesteroloemia
codelist <- read_excel(path = file_path, sheet = "outcome12") |>
  pull(concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "hypercholesteroloemia") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")


## acute myocardial infarction
codelist <- read_excel(path = file_path, sheet = "outcome16") |>
  pull(concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "acute_myocardial_infarction") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")

##  ischemic stroke
codelist <- read_excel(path = file_path, sheet = "outcome17") |>
  pull(concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "ischemic_stroke") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")

# cause of death ----
out_path <- "~/ProstateCancerTTE/Codelist/CauseOfDeath"
## prostate-cancer specific mortality ----

codelist <- read_excel(path = file_path, sheet = "outcome15") |>
  pull(cause_concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "prostate_cancer_death") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")


## cardiovascular diseases specific mortality ----

codelist <- read_excel(path = file_path, sheet = "outcome18") |>
  pull(cause_concept_id) |>
  as.integer() |>
  list() |>
  set_names(nm = "cvd_death") |>
  newCodelist()

exportCodelist(x = codelist, path = out_path, type = "csv")



# inclusion criteria ----

file_path<- paste0(folder_path,"/optima_pca_codelist_eligibility.xlsx")
out_path <- "~/ProstateCancerTTE/Codelist/InclusionCriteria"
sheets <- excel_sheets(file_path)

for(sheet in sheets){
  x <- list(read_excel(path = file_path, sheet = sheet)$concept_id)
  names(x) <- sheet

  exportCodelist(newCodelist(x), path = out_path , type = "csv")

}

# nco ----

file_path<- paste0(folder_path,"/nco_group.xlsx")
out_path <- "~/ProstateCancerTTE/Codelist/NCO"
sheets <- excel_sheets(file_path)
codelist <- read_excel(path = file_path, sheet = sheets[2]) |>
  dplyr::filter(!(.data$nco %in% c("diverticulosis", "hemorrhoids", "constipation","exclude"))) |>
  dplyr::select(nco, condition_concept_id) |>
  dplyr::group_by(nco) |>
  dplyr::group_split()
names(codelist) <- map_chr(codelist, \(x) unique(x$nco))
codelist <- codelist |>
  map(\(x) as.integer(x$condition_concept_id)) |>
  newCodelist()
exportCodelist(x = codelist, path = out_path, type = "csv")

