
library(readxl)
library(here)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(omopgenerics)

# inclusion criteria ----
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
