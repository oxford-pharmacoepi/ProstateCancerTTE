
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
