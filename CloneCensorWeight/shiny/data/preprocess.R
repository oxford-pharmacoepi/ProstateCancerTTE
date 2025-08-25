
library(readr)
library(here)
library(omopgenerics)
library(dplyr)
library(rlang)
library(purrr)
library(stringr)

codelists <- read_csv(here("..", "Results", "codelists.csv"), show_col_types = FALSE)
result <- importSummarisedResult(path = here("..", "Results"))

# clean results
toTidy <- c("hr_summary", "survival_summary", "events", "followup_summary", "coefficients", "probabilities", "summarise_log_file")
resultType <- unique(settings(result)$result_type)
results <- resultType |>
  set_names() |>
  map(\(rt) {
    res <- filterSettings(result, result_type == rt)
    if (rt %in% toTidy) {
      res <- tidy(res)
      if (identical(unique(res$variable_name), "overall")) {
        res$variable_name <- NULL
      }
      if (identical(unique(res$variable_level), "overall")) {
        res$variable_level <- NULL
      }
      cols <- colnames(res)
      cols <- cols[cols %in% c("time_start", "time_end", "time", "prob_bin")]
      for (col in cols) {
        res[[col]] <- as.numeric(res[[col]])
      }
    }
    res
  })

# order code use
set <- settings(results$code_use)
results$code_use <- results$code_use |>
  mutate(n = suppressWarnings(as.numeric(estimate_value))) |>
  group_by(cdm_name, group_level, variable_level) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  arrange(cdm_name, group_level, desc(total), desc(n)) |>
  select(!c("total", "n")) |>
  newSummarisedResult(settings = set)

# cohort definitions
cohort_definitions <- results$summarise_cohort_attrition |>
  tidy() |>
  select("cohort_name", "reason_id", "reason") |>
  distinct() |>
  mutate(reason_id = as.integer(reason_id)) |>
  arrange(reason_id)
results$cohort_definitions <- unique(cohort_definitions$cohort_name) |>
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
        "Inclusion criteria:",
        paste0(seq_along(definition[-1]), ". ", definition[-1])
      ),
      collapse = "\n\n"
    )
  })

# clean codelists
splitByColumn <- function(x, col) {
  unique(x[[col]]) |>
    set_names() |>
    map(\(val) {
      x |>
        filter(x[[col]] == val) |>
        select(!all_of(col))
    })
}
codelists <- codelists |>
  splitByColumn(col = "codelist_type") |>
  map(\(x) splitByColumn(x = x, col = "codelist_name"))
names(codelists) <- str_to_sentence(str_replace_all(string = names(codelists), pattern = "_", replacement = " "))

save(codelists, results, file = here("data", "shinyData.RData"))
