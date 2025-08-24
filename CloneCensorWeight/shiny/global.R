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

load(file = here("data", "shinyData.RData"))

# cohorts
cohorts <- unique(results$summarise_cohort_count$group_level)
cdms <- unique(results$summarise_cohort_count$cdm_name)

# cohort definitions
cohort_definitions <- results$summarise_cohort_attrition |>
  tidy() |>
  select("cohort_name", "reason_id", "reason") |>
  distinct() |>
  mutate(reason_id = as.integer(reason_id)) |>
  arrange(reason_id)
cohort_definitions <- unique(cohort_definitions$cohort_name) |>
  set_names() |>
  map(\(cn) {
    title <- str_to_sentence(str_replace_all(string = cn, pattern = "_", replacement = " "))
    definition <- cohort_definitions |>
      filter(cohort_name == cn) |>
      pull("reason")
    if (cn %in% c("any_prostate_cancer", "first_prostate_cancer", "surveillance", "prostatectomy", "radiotheraphy")) {
      st <- "Any record of `prostate_cancer`"
    } else if (cn %in% c("any_prostatectomy", "first_prostatectomy")) {
      st <- "Any record of `prostatectomy`"
    } else if (cn %in% c("any_radiotheraphy", "first_radiotheraphy")) {
      st <- "Any record of `radiotheraphy`"
    }
    definition[1] <- st
    definition <- str_replace_all(string = definition, pattern = "`", replacement = "*")
    paste0(
      c(
        paste0("**", title, "**"),
        paste0("Initial qualifying events: ", definition[1]),
        paste0("* ", definition[-1])
      ),
      collapse = "\n\n"
    )
  })
