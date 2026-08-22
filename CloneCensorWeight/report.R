result <- omopgenerics::importSummarisedResult(path = "data")

library(omopgenerics)
library(CohortCharacteristics)
library(gt)

result |>
  filterSettings(result_type == "summarise_characteristics") |>
  filterGroup(cohort_name %in% c("prostatectomy", "radiotheraphy", "prostate_cancer")) |>
  tableCharacteristics(
    header = c("cohort_name", "cdm_name")
  ) |>
  tab_options(
    table.font.size = px(10)
  ) |>
  gtsave("characterisation.pdf")
  
library(ggplot2)
library(dplyr)
x <- result |>
  filterSettings(result_type == "follow_up_time") |>
  tidy() |>
  mutate(time = as.numeric(variable_level), percentage = as.numeric(percentage))
ggplot(data = x, mapping = aes(x = time, y = percentage, colour = cohort_name)) +
  geom_step() +
  facet_grid(~ cdm_name)
ggsave("followup.png")

library(visOmopResults)
x <- result |>
  filterSettings(result_type == "cox_regression") |>
  splitAll() |>
  filter(reference == "untreated") |>
  mutate(
    outcome_type = case_when(
      outcome %in% c("death_cohort", "prostate_cancer_death", "metastasis", "erectile_dysfunction_broad", "incontinence_broad") ~ "Meain Outcome",
      outcome_type == "nco" ~ "Negative Control Outcome",
      .default = "Secondary Outcome"
    )
  ) |>
  arrange(outcome_type, outcome) |>
  relocate(outcome)

x |>
  filter(outcome_type != "Negative Control Outcome") |>
  formatEstimateValue() |>
  formatEstimateName(c("HR" = "<hr> [<hr_lower> - <hr_upper>]"), keepNotFormatted = FALSE) |>
  visTable(
    header = "cdm_name",
    groupColumn = "outcome_type",
    hide = c("result_id", "variable_name", "variable_level", "estimate_type")
  ) |>
  tab_options(
    table.font.size = px(10)
  ) |>
  gtsave("hazzard_ratios.pdf")

library(EmpiricalCalibration)
library(purrr)
p <- x |>
  filter(outcome_type == "Negative Control Outcome") |>
  filter(estimate_name %in% c("coef", "se_coef")) |>
  tidy() |>
  mutate(comparison = paste0(reference, "-", comparator), true = 0) |>
  group_by(comparison, cdm_name) |>
  group_split()
titles <- map(p, \(x) paste0(unique(x$comparison), "; ", unique(x$cdm_name)))
p <- p |>
  map(\(x) {
    plotCiCalibrationEffect(x$coef, x$se_coef, x$true)
  })

library(cowplot)

plot_grid(
  plotlist = p,
  labels = titles,
  label_size = 12,
  label_fontface = "bold"
)

ggsave("nco.png")

