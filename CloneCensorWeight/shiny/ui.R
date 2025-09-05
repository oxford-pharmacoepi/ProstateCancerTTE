
optPicker <- list(`actions-box` = TRUE, `selected-text-format` = "count > 1", `deselect-all-text` = "None", `select-all-text` = "All")

# study ----
studyCard <- markdownToHTML(file = here("study.md"), fragment.only = TRUE) |>
  HTML() |>
  card_body() |>
  card()

# codelists ----
headerCodelists <- div(
  style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px; height: 60px",
  div(
    style = "display:inline-block;",
    span("Select codelist", style = "font-weight: bold; margin-right: 20px;"),
    span("Type:"),
    div(
      pickerInput(
        inputId = "select_codelist_type",
        selected = names(codelists)[1],
        choices = names(codelists),
        multiple = FALSE,
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
        multiple = FALSE,
        options = optPicker,
        width = "150px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    ),
    downloadButton(outputId = "download_codelist", label = "Download codelist")
  )
)

# codelist definition
codelistDefinitionPanel <- nav_panel(
  title = span(
    "Codelist definition",
    popover(
      icon("circle-info"),
      title = "Codelist definition",
      "This panel contains the standard concept ids included in each codelist definition",
      options = list(trigger = "hover")
    )
  ),
  card(reactableOutput(outputId = "codelist_definition"))
)

# codelist code use
codelistCodeUsePanel <- nav_panel(
  title = span(
    "Codelist code use",
    popover(
      icon("circle-info"),
      title = "Codelist code use",
      "This panel contains the record count for each standard-source concept id pair for a desired codelist.",
      options = list(trigger = "hover")
    )
  ),
  card(gt_output(outputId = "codelist_code_use"))
)

# cohorts ----
headerCohorts <- div(
  style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px; height: 60px",
  div(
    style = "display:inline-block;",
    span("Select cohort(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cohort",
        selected = cohorts[1],
        choices = cohorts,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    ),
    span("Select database(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cdm_cohort",
        selected = cdms,
        choices = cdms,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    )
  )
)

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
  card(uiOutput(outputId = "ui_cohort_definition"))
)

# cohort count
cohortCountPanel <- nav_panel(
  title = span(
    "Cohort count",
    popover(
      icon("circle-info"),
      title = "Cohort count",
      "This panel contains the counts of each one of the cohorts",
      options = list(trigger = "hover")
    )
  ),
  card(
    card_header(downloadButton(outputId = "dwn_cohort_count")),
    gt_output(outputId = "cohort_count")
  )
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
  card(
    card_header(downloadButton(outputId = "dwn_cohort_attrition")),
    grVizOutput(outputId = "cohort_attrition")
  )
)

# cohort overlap
cohortOverlapPanel <- nav_panel(
  title = span(
    "Cohort overlap",
    popover(
      icon("circle-info"),
      title = "Cohort overlap",
      "This panel contains the overlap of each one of the cohorts",
      options = list(trigger = "hover")
    )
  ),
  card(
    plotOutput(outputId = "plt_overlap")
  )
)

# cohort timing
cohortTimingPanel <- nav_panel(
  title = span(
    "Cohort timing",
    popover(
      icon("circle-info"),
      title = "Cohort timing",
      "This panel contains the timing between the first entries of the cohorts",
      options = list(trigger = "hover")
    )
  ),
  card(
    Toggle.shinyInput(
      label = NULL,
      onText = "Cumulative",
      offText = "Density",
      value = FALSE,
      inputId = "plt_timing_cumulative"
    ),
    plotlyOutput(outputId = "plt_timing")
  )
)

# characterisation ----
headerCharacteristics <- div(
  style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px; height: 60px",
  div(
    style = "display:inline-block;",
    span("Select cohort(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cohort_char",
        selected = cohorts[1],
        choices = cohorts,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    ),
    span("Select database(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cdm_cohort_char",
        selected = cdms,
        choices = cdms,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    )
  )
)

characteristicsPanel <- nav_panel(
  title = span(
    "Cohort characteristics",
    popover(
      icon("circle-info"),
      title = "Cohort characteristics",
      "This panel contains the characterisation for the cohorts of interest",
      options = list(trigger = "hover")
    )
  ),
  navset_card_tab(
    nav_panel(
      title = "Table characteristics",
      gt_output(outputId = "cohort_characteristics")
    ),
    nav_panel(
      title = "Plot characteristics",
      layout_sidebar(
        sidebar = sidebar(
          pickerInput(
            inputId = "plt_char_variable",
            label = "Variable",
            choices = plt_char_var,
            selected = plt_char_var[1],
            multiple = FALSE
          ),
          pickerInput(
            inputId = "plt_char_facet",
            label = "Facet",
            choices = c("cohort_name", "cdm_name"),
            selected = "cdm_name",
            multiple = TRUE
          ),
          pickerInput(
            inputId = "plt_char_colour",
            label = "Colour",
            choices = c("cohort_name", "cdm_name"),
            selected = "cohort_name",
            multiple = TRUE
          )
        ),
        plotOutput(outputId = "plt_characteristics")
      )
    )
  )
)

lscPanel <- nav_panel(
  title = span(
    "Large scale characteristics",
    popover(
      icon("circle-info"),
      title = "Large scale characteristics",
      "This panel contains the large scale characterisation that is a data driven characterisation at concept ID level.",
      options = list(trigger = "hover")
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      position = "left",
      pickerInput(
        inputId = "lsc_window",
        label = "Window",
        choices = windows,
        selected = windows,
        multiple = TRUE
      ),
      pickerInput(
        inputId = "lsc_table_name",
        label = "Table name",
        choices = table_names,
        selected = table_names,
        multiple = TRUE
      )
    ),
    navset_card_tab(
      nav_panel(
        title = "Explore LSC data",
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            pickerInput(
              inputId = "lsc_compare_by",
              label = "Compare by",
              choices = c("no SMD", "cdm_name", "cohort_name", "window"),
              selected = "no SMD",
              multiple = FALSE
            ),
            pickerInput(
              inputId = "lsc_smd_reference",
              label = "SMD reference",
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            )
          ),
          reactableOutput(outputId = "tbl_lsc_explore")
        )
      ),
      nav_panel(
        title = "Top identified concepts",
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            pickerInput(
              inputId = "lsc_top_concepts",
              label = "Top concepts",
              choices = c(1, 5, 10, 25, 50, 100),
              selected = 10,
              multiple = FALSE
            )
          ),
          gt_output(outputId = "tbl_lsc_top_concepts")
        )
      ),
      nav_panel(
        title = "Plot large scale comparison",
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            Toggle.shinyInput(
              label = "Missing data",
              onText = "Interpolate 0",
              offText = "Eliminate",
              value = TRUE,
              inputId = "plt_lsc_missings"
            ),
            pickerInput(
              inputId = "plt_lsc_colour",
              label = "Colour",
              choices = c("cdm_name", "cohort_name", "window"),
              selected = "window",
              multiple = FALSE,
              options = optPicker
            ),
            pickerInput(
              inputId = "plt_lsc_reference",
              label = "Reference",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = optPicker
            ),
            pickerInput(
              inputId = "plt_lsc_facet",
              label = "Facet",
              choices = c("cdm_name", "cohort_name", "window"),
              selected = c("cdm_name", "cohort_name"),
              multiple = TRUE,
              options = optPicker
            )
          ),
          plotlyOutput(outputId = "plt_lsc_compared")
        )
      )
    )
  )
)

# diagnostics ----
headerDiagnostics <- div(
  style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px; height: 60px",
  div(
    style = "display:inline-block;",
    span("Select database(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cdm_diag",
        selected = cdms,
        choices = cdms,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    ),
    span("Select outcome(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_out_diag",
        selected = outcomes[1],
        choices = outcomes,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    )
  )
)

# number subjects
subjectsPanel <- nav_panel(
  title = span(
    "Number subjects",
    popover(
      icon("circle-info"),
      title = "Number subjects",
      "Number subjects (weigthed or crude) over time.",
      options = list(trigger = "hover")
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      Toggle.shinyInput(
        label = "Weights",
        onText = "Weighted",
        offText = "Not weighted",
        value = TRUE,
        inputId = "weighted_sub"
      ),
      pickerInput(
        label = "Colour",
        inputId = "colour_sub",
        choices = c("cdm_name", "cohort_name", "outcome"),
        selected = c("cohort_name"),
        multiple = TRUE
      ),
      pickerInput(
        label = "Facet",
        inputId = "facet_sub",
        choices = c("cdm_name", "cohort_name", "outcome"),
        selected = c("cdm_name", "outcome"),
        multiple = TRUE
      )
    ),
    plotOutput(outputId = "plt_subjects")
  )
)

# number events
eventsPanel <- nav_panel(
  title = span(
    "Number events",
    popover(
      icon("circle-info"),
      title = "Number events",
      "Number events (weigthed or crude) for each one of the outcomes of interest.",
      options = list(trigger = "hover")
    )
  ),
  Toggle.shinyInput(
    label = "Weights",
    onText = "Weighted",
    offText = "Not weighted",
    value = FALSE,
    inputId = "weighted_events"
  ),
  gt_output(outputId = "gt_events")
)

# follow-up
followUpPanel <- nav_panel(
  title = span(
    "Follow up",
    popover(
      icon("circle-info"),
      title = "Follow up",
      "Number of days of follow up for each one of the cohorts of interest.",
      options = list(trigger = "hover")
    )
  ),
  gt_output(outputId = "gt_followup")
)

# coefficients
coefPanel <- nav_panel(
  title = span(
    "Coefficients",
    popover(
      icon("circle-info"),
      title = "Coefficients",
      "Coefficients of the PS model to calculate the weights",
      options = list(trigger = "hover")
    )
  ),
  reactableOutput(outputId = "tbl_coef")
)

# ps model
psPanel <- nav_panel(
  title = span(
    "PS model",
    popover(
      icon("circle-info"),
      title = "PS model",
      "Propensity scores distribution in each cohort",
      options = list(trigger = "hover")
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      pickerInput(
        inputId = "plt_ps_times",
        label = "Weight time",
        choices = times,
        selected = times,
        multiple = TRUE,
        options = optPicker
      ),
      Toggle.shinyInput(
        label = "cdm_name",
        onText = "cols",
        offText = "rows",
        value = TRUE,
        inputId = "plt_ps_cdm_name"
      ),
      Toggle.shinyInput(
        label = "cohort_name",
        onText = "cols",
        offText = "rows",
        value = TRUE,
        inputId = "plt_ps_cohort_name"
      ),
      Toggle.shinyInput(
        label = "outcome",
        onText = "cols",
        offText = "rows",
        value = FALSE,
        inputId = "plt_ps_outcome"
      ),
      Toggle.shinyInput(
        label = "Weights time",
        onText = "cols",
        offText = "rows",
        value = FALSE,
        inputId = "plt_ps_time"
      )
    ),
    plotOutput(outputId = "plt_ps")
  )
)

# weights panel
weightPanel <- nav_panel(
  title = span(
    "Weights",
    popover(
      icon("circle-info"),
      title = "Weights",
      "Weights distribution in each of the arms.",
      options = list(trigger = "hover")
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      pickerInput(
        inputId = "plt_we_times",
        label = "Weight time",
        choices = times,
        selected = times,
        multiple = TRUE,
        options = optPicker
      ),
      Toggle.shinyInput(
        label = "cdm_name",
        onText = "cols",
        offText = "rows",
        value = TRUE,
        inputId = "plt_we_cdm_name"
      ),
      Toggle.shinyInput(
        label = "outcome",
        onText = "cols",
        offText = "rows",
        value = FALSE,
        inputId = "plt_we_outcome"
      ),
      Toggle.shinyInput(
        label = "Weights time",
        onText = "cols",
        offText = "rows",
        value = FALSE,
        inputId = "plt_we_time"
      )
    ),
    plotOutput(outputId = "plt_we")
  )
)

# reaults ----
headerResults <- div(
  style = "background-color: var(--bs-primary); color: white; padding: 10px; font-weight: normal; gap: 10px; height: 60px",
  div(
    style = "display:inline-block;",
    span("Select database(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_cdm_result",
        selected = cdms,
        choices = cdms,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    ),
    span("Select outcome(s)", style = "font-weight: bold; margin-right: 20px;"),
    div(
      pickerInput(
        inputId = "select_out_result",
        selected = outcomes[1],
        choices = outcomes,
        multiple = TRUE,
        options = optPicker,
        width = "250px"
      ),
      style = "display:inline-block; margin-right: 20px;"
    )
  )
)

# kaplan meier
kmPanel <- nav_panel(
  title = span(
    "Kaplan Meier",
    popover(
      icon("circle-info"),
      title = "Kaplan Meier",
      "Kaplan Meier plots to show the survival distribution",
      options = list(trigger = "hover")
    )
  ),
  card(
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        Toggle.shinyInput(
          label = "cdm_name",
          onText = "cols",
          offText = "rows",
          value = TRUE,
          inputId = "plt_km_cdm_name"
        ),
        Toggle.shinyInput(
          label = "outcome",
          onText = "cols",
          offText = "rows",
          value = FALSE,
          inputId = "plt_km_outcome"
        )
      ),
      plotOutput(outputId = "plt_res_km")
    )
  )
)

# hazard ratios
hrPanel <- nav_panel(
  title = span(
    "Hazard ratios",
    popover(
      icon("circle-info"),
      title = "Hazard ratios",
      "Hazard ratio forest plots",
      options = list(trigger = "hover")
    )
  ),
  card(
    div(
      style = "display:inline-block;",
      span("Reference", style = "font-weight: bold; margin-right: 20px;"),
      div(
        pickerInput(
          inputId = "plt_result_reference",
          choices = cohorts_out,
          selected = cohorts_out[1],
          multiple = FALSE
        ),
        style = "display:inline-block; margin-right: 20px;"
      )
    ),
    plotOutput(outputId = "plt_result")
  )
)

# ui ----
ui <- page_navbar(
  title = "Clone-Censor-Weight",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(title = "Study", studyCard),
  nav_panel(
    title = "Codelists",
    headerCodelists,
    navset_card_tab(
      full_screen = TRUE,
      title = "Codelists",
      codelistDefinitionPanel,
      codelistCodeUsePanel
    )
  ),
  nav_panel(
    title = "Cohorts",
    headerCohorts,
    navset_card_tab(
      full_screen = TRUE,
      title = "Cohorts",
      cohortDefinitionPanel,
      cohortCountPanel,
      cohortAttritionPanel,
      cohortOverlapPanel,
      cohortTimingPanel
    )
  ),
  nav_panel(
    title = "Characterisation",
    headerCharacteristics,
    navset_card_tab(
      full_screen = TRUE,
      title = "Characterisation",
      characteristicsPanel,
      lscPanel
    )
  ),
  nav_panel(
    title = "Diagnostics",
    headerDiagnostics,
    navset_card_tab(
      full_screen = TRUE,
      title = "Diagnostics",
      subjectsPanel,
      eventsPanel,
      followUpPanel,
      coefPanel,
      psPanel,
      weightPanel
    )
  ),
  nav_panel(
    title = "Results",
    headerResults,
    navset_card_tab(
      full_screen = TRUE,
      title = "Results",
      kmPanel,
      hrPanel
    )
  ),
  nav_spacer(),
  nav_item(a(
    icon("github"),
    href = "https://github.com/oxford-pharmacoepi/ProstateCancerTTE",
    target = "_blank"
  ))
)
