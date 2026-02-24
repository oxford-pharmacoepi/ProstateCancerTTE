# shiny is prepared to work with this resultList:
resultList <- list(
  events_summary = list(result_type = "events_summary"),
  followup_summary = list(result_type = "followup_summary"),
  survival_summary = list(result_type = "survival_summary"),
  hr_summary = list(result_type = "hr_summary"), 
  selected_features = list(result_type = "selected_features"),
  distribution_ps = list(result_type = "distribution_ps"),
  asmd = list(result_type = "asmd"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"), 
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  nco_events_summary = list(result_type = "nco_events_summary"),
  nco_followup_summary = list(result_type = "nco_followup_summary"),
  nco_survival_summary = list(result_type = "nco_survival_summary"),
  nco_hr_summary = list(result_type = "nco_hr_summary")
)

source(file.path(getwd(), "functions.R"))

data_path <- file.path(getwd(), "data")
csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

result <- purrr::map(csv_files, \(x){
  omopgenerics::importSummarisedResult(x)
}) |> 
  omopgenerics::bind()


data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

v <- values$asmd_variable_level

# extract variable names (before first underscore)
variable_names <- sub("_.*", "", v)

# extract window (everything after first underscore)
window <- ifelse(grepl("_", v), sub("^[^_]+_", "", v), "0") |> unique()

# optional: store them back into choices
values$asmd_variable_level <- variable_names
values$asmd_window <- window

# edit choices and values of interest
choices <- values
selected <- getSelected(values)



save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
