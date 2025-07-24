
optPicker <- list(`actions-box` = TRUE, `selected-text-format` = "count > 1", `deselect-all-text` = "None", `select-all-text` = "All")

# study
studyCard <- markdownToHTML(file = here("study.md"), fragment.only = TRUE) |>
  HTML() |>
  card_body() |>
  card()

# cohort definition
cohortDefinitionPanel <- nav_panel(
  title = span(
    "Cohort definition",
    popover(
      icon("circle-info"),
      title = "Cohort definition",
      "This panel contains the definition of each one of the cohorts",
      options = list(trigger = "hover")
    )
  ),
  card_title("A plotly plot"),
  "fghj"
)

# cohort attrition
cohortAttritionPanel <- nav_panel(
  title = span(
    "Cohort attrition",
    popover(
      icon("circle-info"),
      title = "Cohort attrition",
      "This panel contains the attrition of each one of the cohorts",
      options = list(trigger = "hover")
    )
  ),
  card_title("A plotly plot"),
  "fghj"
)

ui <- page_navbar(
  title = "Clone-Censor-Weight",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(title = "Study", studyCard),
  nav_panel(
    title = "Codelists",
    div(
      height = "100px",
      style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px;",
      div(
        style = "display:inline-block;",
        span("Select codelist", style = "font-weight: bold; margin-right: 20px;"),
        span("Type:"),
        div(
          pickerInput(
            inputId = "select_codelist_type",
            selected = "index",
            choices = c("index", "outcome", "exclude", "characterisation_conditions", "characterisation_medications"),
            multiple = TRUE,
            options = optPicker,
            width = "150px"
          ),
          style = "display:inline-block; margin-right: 20px;"
        ),
        span("Codelist:"),
        div(
          pickerInput(
            inputId = "select_codelist",
            selected = NULL,
            choices = NULL,
            multiple = TRUE,
            options = optPicker,
            width = "150px"
          ),
          style = "display:inline-block; margin-right: 20px;"
        ),
        actionButton(
          inputId = "update_codelists",
          label = "Update",
          style = "background-color: var(--bs-secondary); color: white;"
        )
      )
    ),
    card(
      reactableOutput(outputId = "codelists")
    )
  ),
  nav_panel(
    title = "Cohorts",
    navset_card_tab(
      full_screen = TRUE,
      title = "Cohorts",
      cohortDefinitionPanel,
      nav_panel(
        title = "Cohort count",
        card_title("A leaflet plot"),
        "fghjmkd"
      ),
      cohortAttritionPanel
    )
  ),
  nav_panel(
    title = "Characterisation",
    navset_bar(
      nav_panel(
        title = "Cohort Characteristics"
      ),
      nav_panel(
        title = "Large Scale Characteristics"
      )
    )
  ),
  nav_panel(
    title = "Diagnostics",
    navset_bar(
      nav_panel(
        title = "Number subjects"
      ),
      nav_panel(
        title = "Events"
      ),
      nav_panel(
        title = "Follow-up"
      ),
      nav_panel(
        title = "PS model coefficients"
      ),
      nav_panel(
        title = "PS model result"
      ),
      nav_panel(
        title = "Weights"
      ),
      nav_panel(
        title = "Number subjects"
      )
    )
  ),
  nav_panel(
    title = "Results",
    navset_bar(
      nav_panel(
        title = "Kaplan-Meier"
      ),
      nav_panel(
        title = "Hazard Ratio"
      )
    )
  ),
  nav_spacer(),
  nav_item(a(
    icon("github"),
    href = "https://github.com/oxford-pharmacoepi/ProstateCancerTTE",
    target = "_blank"
  ))
)
