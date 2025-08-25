# copy files
file.copy(from = here::here("..", "figures", "diagram.png"), to = here::here("www", "diagram.png"), overwrite = TRUE)
file.copy(from = system.file("oxford.png", package = "OmopViewer"), to = here::here("www", "oxford.png"), overwrite = TRUE)
file.copy(from = system.file("logos/hds_logo.svg", package = "OmopViewer"), to = here::here("www", "hds_logo.svg"), overwrite = TRUE)

library(bslib)
library(shiny)
library(here)
library(readr)
library(purrr)
library(tools)
library(stringr)
library(shinyWidgets)
library(reactable)
library(markdown)
library(gt)
library(CohortCharacteristics)
library(DiagrammeR)
library(omopgenerics)
library(dplyr)
library(CodelistGenerator)

load(file = here("data", "shinyData.RData"))

# variables
cohorts <- unique(results$summarise_cohort_count$group_level)
cdms <- unique(results$summarise_cohort_count$cdm_name)
