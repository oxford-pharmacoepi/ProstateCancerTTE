
library(readr)
library(here)
library(omopgenerics)
library(dplyr)
library(rlang)
library(purrr)
library(stringr)
library(omock)
library(tidyr)

codelists <- read_csv(here("..", "Results", "codelists.csv"), show_col_types = FALSE)
result <- importSummarisedResult(path = here("..", "Results"))

# clean results
toTidy <- c("hr_summary", "survival_summary", "events", "coefficients", "probabilities", "summarise_log_file")
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

# characterisation
results$summarise_characteristics <- results$summarise_characteristics |>
  filter(!variable_name %in% c("Cohort start date", "Cohort end date", "Days in cohort")) |>
  filter(!estimate_name %in% c("mean", "sd")) |>
  mutate(
    estimate_value = case_when(
      variable_name %in% c("Future observation", "Prior observation") ~ sprintf("%.1f", suppressWarnings(as.numeric(estimate_value) / 365)),
      .default = estimate_value
    ),
    estimate_type = case_when(
      variable_name %in% c("Future observation", "Prior observation") ~ "numeric",
      variable_name == "Visit counts in the year prior" ~ "integer",
      .default = estimate_type
    ),
    variable_level = case_when(
      variable_name %in% c("Number records", "Number subjects", "Age") ~ variable_name,
      variable_name %in% c("Prior observation", "Future observation") ~ paste0(variable_name, " (years)"),
      variable_name == "Age group" ~ paste0("Age group: ", variable_level),
      variable_name == "Sex" ~ "Sex: Male",
      variable_level == "None" ~ "None recorded",
      .default = variable_level
    ),
    variable_name = case_when(
      variable_name %in% c("Number records", "Number subjects") ~ "Counts",
      variable_name == "Diagnostic" ~ "Diagnostic source",
      variable_name %in% c("Sex", "Age", "Age group", "Prior observation", "Future observation") ~ "Demographics",
      .default = variable_name
    )
  ) |>
  inner_join(
    tibble(
      variable_name = c("Counts", "Demographics", "Diagnostic source", "Conditions any time prior", "Medications year prior"),
      order_id_1 = 1:5L
    ),
    by = "variable_name"
  ) |>
  left_join(
    tibble(
      variable_level = c(
        "Primary care", "Hospital", "Registry", "Primary care and hospital",
        "Primary care and registry", "Hospital and registry",
        "Primary care, registry and hospital", "None recorded"
      ),
      order_id_2 = 1:8L
    ),
    by = "variable_level"
  ) |>
  mutate(order_id_2 = coalesce(order_id_2, 0L)) |>
  arrange(group_level, order_id_1, order_id_2) |>
  select(!c("order_id_1", "order_id_2"))

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

# coefficients
lc <- mockCdmFromDataset(datasetName = "empty_cdm")
results$coefficients <- results$coefficients |>
  mutate(concept_id = as.integer(str_match(covariate, "^cov_(\\d+)$")[,2])) |>
  left_join(
    lc$concept |>
      select("concept_id", "concept_name"),
    by = "concept_id"
  ) |>
  mutate(
    concept_name = coalesce(concept_name, covariate),
    weights_time = as.integer(time_start)
  ) |>
  select(
    "cdm_name", "cohort_name" = "group", "outcome", "weights_time",
    "covariate" = "concept_name", "concept_id", "value"
  )

# number subjects
results$survival_summary <- results$survival_summary |>
  group_by(cdm_name, cohort_name, outcome) |>
  mutate(group_id = cur_group_id()) |>
  ungroup()

# follow up
results$followup_summary <- results$followup_summary |>
  tidy() |>
  group_by(cdm_name, cohort_name, outcome) |>
  mutate(percentage = 100 * n / max(n)) |>
  ungroup() |>
  mutate(across(c("min", "q05", "q25", "median", "q75", "q95", "max"), as.integer)) |>
  select(!"n") |>
  transformToSummarisedResult(
    group = "cohort_name",
    strata = "outcome",
    additional = "reason",
    estimates = c("percentage", "min", "q05", "q25", "median", "q75", "q95", "max")
  )

# coefficients
results$coefficients <- results$coefficients |>
  filter(covariate != "(Intercept)") |>
  pivot_wider(names_from = "cohort_name", values_from = "value")

# probabilities
results$probabilities <- results$probabilities |>
  group_by(cdm_name, cohort_name, outcome, prob_label) |>
  mutate(group_id = cur_group_id()) |>
  ungroup()

# hazard ratio
x <- results$hr_summary |>
  select("cdm_name", "reference", "comparator", "outcome", "interval", "coef", "se_coef")
results$hr_summary <- x |>
  union_all(
    x |>
      rename("reference" = "comparator", "comparator" = "reference") |>
      mutate(coef = -coef)
  ) |>
  mutate(
    hr = exp(coef),
    hr_lower = exp(coef - 1.96 * se_coef),
    hr_upper = exp(coef + 1.96 * se_coef),
    across(
      .cols = c("hr", "hr_lower", "hr_upper"),
      .fns = \(x) case_when(x <= 0.01 ~ 0.01, x>= 100 ~ 100, .default = x)
    ),
    comparator = factor(comparator, levels = c("surveillance", "prostatectomy", "radiotheraphy")),
    interval = factor(interval, levels = c("overall", "first year", "second year")),
    interval_num = as.numeric(interval) - 1
  ) |>
  select(!c("coef", "se_coef")) |>
  group_by(cdm_name, outcome, reference) |>
  arrange(interval, comparator) |>
  mutate(y = - row_number() - 0.5 * interval_num) |>
  ungroup()

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
