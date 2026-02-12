update_button_card <- function(id, message_id) {
  bslib::card(
    bslib::card_body(
      shiny::actionButton(
        inputId = id,
        label = "Update content",
        width = "100%"
      ),
      shiny::uiOutput(outputId = message_id)
    )
  )
}

## UI ------------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = shiny::tags$span(
    shiny::tags$img(
      src = "CopyOfoptima.png",
      width = "auto",
      height = "46px",
      class = "me-3",
      alt = "logo"
    ),
    ""
  ),
  theme = bslib::bs_theme(bootswatch = "simplex"),
  
  ## Background
  bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("book-atlas"),
    backgroundCard("background.md")
  ),
  
  ## Cohort Characteristics (moved before Lasso and PS)
  bslib::nav_menu(
    title = "Cohort Characteristics",
    icon = shiny::icon("list"),
    bslib::nav_panel(
      title = "Cohort Attrition",
      icon = shiny::icon("layer-group"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            class = "sticky-top-btn",
            bslib::card_body(
              shiny::actionButton(
                inputId = "update_summarise_cohort_attrition",
                label = "Update content",
                width = "100%"
              ),
              uiOutput(outputId = "update_message_summarise_cohort_attrition")
            )
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_attrition_cdm_name",
            label = "CDM name",
            choices = choices$summarise_cohort_attrition_cdm_name,
            selected = selected$summarise_cohort_attrition_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_attrition_cohort_name",
            label = "Cohort name",
            choices = choices$summarise_cohort_attrition_cohort_name,
            selected = selected$summarise_cohort_attrition_cohort_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_attrition_variable_name",
            label = "Variable name",
            choices = choices$summarise_cohort_attrition_variable_name,
            selected = selected$summarise_cohort_attrition_variable_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Table Attrition",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_attrition_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_attrition_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  sortable::bucket_list(
                    header = NULL,
                    sortable::add_rank_list(
                      text = "None",
                      labels = "reason",
                      input_id = "summarise_cohort_attrition_table_none"
                    ),
                    sortable::add_rank_list(
                      text = "Header",
                      labels = "variable_name",
                      input_id = "summarise_cohort_attrition_table_header"
                    ),
                    sortable::add_rank_list(
                      text = "Group columns",
                      labels = c("cdm_name", "cohort_name"),
                      input_id = "summarise_cohort_attrition_table_group_column"
                    ),
                    sortable::add_rank_list(
                      text = "Hide",
                      labels = c("variable_level", "reason_id", "estimate_name", "cdm_version", "cohort_definition_id", "table_name", "vocabulary_version"),
                      input_id = "summarise_cohort_attrition_table_hide"
                    )
                  ),
                  position = "right"
                ),
                gt::gt_output("summarise_cohort_attrition_table") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Diagram",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::numericInput(
                    inputId = "summarise_cohort_attrition_diagram_width",
                    label = "Width (px)",
                    value = 2000
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_attrition_diagram_download", label = "Download Diagram")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_attrition_diagram_show",
                    label = "Show",
                    choices = c("subjects", "records"),
                    selected = c("subjects", "records"),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                DiagrammeR::grVizOutput("summarise_cohort_attrition_diagram") |>
                  shinycssloaders::withSpinner()
              )
            )
          )
        )
      )
    ),bslib::nav_panel(
      title = "Cohort Count",
      icon = shiny::icon("users"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            class = "sticky-top-btn",
            bslib::card_body(
              shiny::actionButton(
                inputId = "update_summarise_cohort_count",
                label = "Update content",
                width = "100%"
              ),
              uiOutput(outputId = "update_message_summarise_cohort_count")
            )
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_count_cdm_name",
            label = "CDM name",
            choices = choices$summarise_cohort_count_cdm_name,
            selected = selected$summarise_cohort_count_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_count_cohort_name",
            label = "Cohort name",
            choices = choices$summarise_cohort_count_cohort_name,
            selected = selected$summarise_cohort_count_cohort_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_count_variable_name",
            label = "Variable name",
            choices = choices$summarise_cohort_count_variable_name,
            selected = selected$summarise_cohort_count_variable_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_cohort_count_table_name",
            label = "Table name",
            choices = choices$summarise_cohort_count_table_name,
            selected = selected$summarise_cohort_count_table_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Table Counts",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_count_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  sortable::bucket_list(
                    header = NULL,
                    sortable::add_rank_list(
                      text = "None",
                      labels = c("variable_name", "estimate_name"),
                      input_id = "summarise_cohort_count_table_none"
                    ),
                    sortable::add_rank_list(
                      text = "Header",
                      labels = "cohort_name",
                      input_id = "summarise_cohort_count_table_header"
                    ),
                    sortable::add_rank_list(
                      text = "Group columns",
                      labels = "cdm_name",
                      input_id = "summarise_cohort_count_table_group_column"
                    ),
                    sortable::add_rank_list(
                      text = "Hide",
                      labels = c("variable_level", "table_name"),
                      input_id = "summarise_cohort_count_table_hide"
                    )
                  ),
                  position = "right"
                ),
                gt::gt_output("summarise_cohort_count_table") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Plot Counts",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::numericInput(
                    inputId = "summarise_cohort_count_plot_width",
                    label = "Width",
                    value = 15
                  ),
                  shiny::numericInput(
                    inputId = "summarise_cohort_count_plot_height",
                    label = "Height",
                    value = 15
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_plot_units",
                    label = "Units",
                    choices = c("px", "cm", "inch"),
                    selected = "cm",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::numericInput(
                    inputId = "summarise_cohort_count_plot_dpi",
                    label = "DPI",
                    value = 300
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_count_plot_download", label = "Download plot")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::materialSwitch(
                    inputId = "summarise_cohort_count_plot_interactive",
                    label = "Interactive",
                    value = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_plot_facet",
                    label = "Facet",
                    choices = c("cdm_name", "cohort_name", "table_name"),
                    selected = "cdm_name",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_plot_colour",
                    label = "Colour",
                    choices = c("cdm_name", "cohort_name", "table_name"),
                    selected = "cohort_name",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                shiny::uiOutput("summarise_cohort_count_plot") |>
                  shinycssloaders::withSpinner()
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Cohort Characteristics",
      icon = shiny::icon("users-gear"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            class = "sticky-top-btn",
            bslib::card_body(
              shiny::actionButton(
                inputId = "update_summarise_characteristics",
                label = "Update content",
                width = "100%"
              ),
              uiOutput(outputId = "update_message_summarise_characteristics")
            )
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_characteristics_cdm_name",
            label = "CDM name",
            choices = choices$summarise_characteristics_cdm_name,
            selected = selected$summarise_characteristics_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_characteristics_cohort_name",
            label = "Cohort name",
            choices = choices$summarise_characteristics_cohort_name,
            selected = selected$summarise_characteristics_cohort_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_characteristics_variable_name",
            label = "Variable name",
            choices = choices$summarise_characteristics_variable_name,
            selected = selected$summarise_characteristics_variable_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_characteristics_estimate_name",
            label = "Estimate name",
            choices = choices$summarise_characteristics_estimate_name,
            selected = selected$summarise_characteristics_estimate_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Table Characteristics",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "summarise_characteristics_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  sortable::bucket_list(
                    header = NULL,
                    sortable::add_rank_list(
                      text = "None",
                      labels = c("variable_name", "variable_level", "estimate_name"),
                      input_id = "summarise_characteristics_table_none"
                    ),
                    sortable::add_rank_list(
                      text = "Header",
                      labels = c("cdm_name", "cohort_name"),
                      input_id = "summarise_characteristics_table_header"
                    ),
                    sortable::add_rank_list(
                      text = "Group columns",
                      labels = character(),
                      input_id = "summarise_characteristics_table_group_column"
                    ),
                    sortable::add_rank_list(
                      text = "Hide",
                      labels = "table_name",
                      input_id = "summarise_characteristics_table_hide"
                    )
                  ),
                  position = "right"
                ),
                gt::gt_output("summarise_characteristics_table") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Plot Characteristics",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::numericInput(
                    inputId = "summarise_characteristics_plot_width",
                    label = "Width",
                    value = 15
                  ),
                  shiny::numericInput(
                    inputId = "summarise_characteristics_plot_height",
                    label = "Height",
                    value = 15
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_units",
                    label = "Units",
                    choices = c("px", "cm", "inch"),
                    selected = "cm",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::numericInput(
                    inputId = "summarise_characteristics_plot_dpi",
                    label = "DPI",
                    value = 300
                  ),
                  shiny::downloadButton(outputId = "summarise_characteristics_plot_download", label = "Download plot")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::materialSwitch(
                    inputId = "summarise_characteristics_plot_interactive",
                    label = "Interactive",
                    value = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_plot_type",
                    label = "Plot type",
                    choices = c("boxplot", "barplot", "scatterplot"),
                    selected = "boxplot",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_facet",
                    label = "Facet",
                    choices = c("cdm_name", "cohort_name"),
                    selected = "cdm_name",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_colour",
                    label = "Colour",
                    choices = c("cdm_name", "cohort_name"),
                    selected = "cohort_name",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                shiny::uiOutput("summarise_characteristics_plot") |>
                  shinycssloaders::withSpinner()
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Large Scale Characteristics",
      icon = shiny::icon("arrow-up-right-dots"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            class = "sticky-top-btn",
            bslib::card_body(
              shiny::actionButton(
                inputId = "update_summarise_large_scale_characteristics",
                label = "Update content",
                width = "100%"
              ),
              uiOutput(outputId = "update_message_summarise_large_scale_characteristics")
            )
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_cdm_name",
            label = "CDM name",
            choices = choices$summarise_large_scale_characteristics_cdm_name,
            selected = selected$summarise_large_scale_characteristics_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_cohort_name",
            label = "Cohort name",
            choices = choices$summarise_large_scale_characteristics_cohort_name,
            selected = selected$summarise_large_scale_characteristics_cohort_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_variable_level",
            label = "Variable level",
            choices = choices$summarise_large_scale_characteristics_variable_level,
            selected = selected$summarise_large_scale_characteristics_variable_level,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_analysis",
            label = "Analysis",
            choices = choices$summarise_large_scale_characteristics_analysis,
            selected = selected$summarise_large_scale_characteristics_analysis,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_table_name",
            label = "Table name",
            choices = choices$summarise_large_scale_characteristics_table_name,
            selected = selected$summarise_large_scale_characteristics_table_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "summarise_large_scale_characteristics_type",
            label = "Type",
            choices = choices$summarise_large_scale_characteristics_type,
            selected = selected$summarise_large_scale_characteristics_type,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Table",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::downloadButton(outputId = "summarise_large_scale_characteristics_table_lsc_download", label = "Download table")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_table_lsc_compare_by",
                    label = "Compare by",
                    choices = c("no compare", "cdm_name", "cohort_name", "type", "variable_level"),
                    selected = "no compare",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_table_lsc_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "type", "variable_level"),
                    selected = "type",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_table_lsc_smd_reference",
                    label = "SMD reference",
                    choices = "no SMD",
                    selected = "no SMD",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                reactable::reactableOutput("summarise_large_scale_characteristics_table_lsc") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Most common codes",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_table_most_common_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "summarise_large_scale_characteristics_table_most_common_download", label = "Download table")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_table_most_common_top_concepts",
                    label = "Top concepts",
                    choices = c(10L, 25L, 100L),
                    selected = 10L,
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                gt::gt_output("summarise_large_scale_characteristics_table_most_common") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Plot Compared",
            bslib::card(
              full_screen = TRUE,
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny.fluent::Toggle.shinyInput(
                    label = "Missing data",
                    onText = "Interpolate 0",
                    offText = "Eliminate",
                    value = TRUE,
                    inputId = "summarise_large_scale_characteristics_plot_compared_missings"
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_plot_compared_colour",
                    label = "Colour",
                    choices = c("cdm_name", "cohort_name", "type", "variable_level"),
                    selected = NULL,
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_plot_compared_reference",
                    label = "Reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_plot_compared_facet",
                    label = "Facet",
                    choices = c("cdm_name", "cohort_name", "type", "variable_level"),
                    selected = c("cdm_name", "cohort_name"),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                plotly::plotlyOutput("summarise_large_scale_characteristics_plot_compared") |>
                  shinycssloaders::withSpinner()
              )
            )
          )
        )
      )
    )
  ), ## end Cohort Characteristics nav_menu
  
  ## Lasso and PS
  bslib::nav_panel(
    title = "Lasso and PS",
    icon = shiny::icon("folder"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        update_button_card("update_lasso_summary", "update_message_lasso_summary"),
        
        shinyWidgets::pickerInput(
          inputId = "lasso_summary_cdm_name",
          label = "CDM name",
          choices = choices$selected_features_cdm_name,
          selected = selected$selected_features_cdm_name,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        ),
        shinyWidgets::pickerInput(
          inputId = "lasso_summary_cohort",
          label = "Cohort",
          choices = choices$selected_features_cohort,
          selected = selected$selected_features_cohort,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        ),
        position = "left",
        width = "300px"
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Selected Features",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::downloadButton(outputId = "selected_features_summary_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::checkboxInput(
                  inputId = "selected_features_summary_tidy_pivot_estimates",
                  label = "Pivot estimates",
                  value = TRUE
                ),
                shinyWidgets::pickerInput(
                  inputId = "lasso_summary_window",
                  label = "Time window",
                  choices = choices$selected_features_window,
                  selected = selected$selected_features_window,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                ),
                position = "right"
              ),
              DT::DTOutput("selected_features_summary_tidy") |>
                shinycssloaders::withSpinner()
            )
          )
        ),
        bslib::nav_panel(
          title = "Distribution PS",
          bslib::card(
            full_screen = TRUE,
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shinyWidgets::pickerInput(
                  inputId = "distribution_ps_facet",
                  label = "Facet",
                  choices = c("cdm_name", "cohort"),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                ),
                position = "right"
              ),
              shiny::plotOutput("distribution_ps_plot", height = 500) |>
                shinycssloaders::withSpinner()
            )
          )
        ),
        bslib::nav_panel(
          title = "ASMD",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::downloadButton(outputId = "asmd_summary_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shinyWidgets::pickerInput(
                  inputId = "asmd_summary_covariate",
                  label = "Covariate",
                  choices = choices$asmd_variable_level,
                  selected = selected$asmd_variable_level,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                ),
                shinyWidgets::pickerInput(
                  inputId = "asmd_summary_window",
                  label = "Time window",
                  choices = choices$asmd_window,
                  selected = selected$asmd_window,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                ),
                shiny::checkboxInput(
                  inputId = "asmd_summary_tidy_pivot_estimates",
                  label = "Pivot estimates",
                  value = TRUE
                ),
                position = "right"
              ),
              DT::DTOutput("asmd_summary_tidy") |>
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  ),
  bslib::nav_menu(
    title = "Outcomes",
    icon = shiny::icon("chart-bar"),
    
    ## Events summary
    bslib::nav_panel(
      title = "Events summary",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_events_summary", "update_message_events_summary"),
          
          shinyWidgets::pickerInput(
            inputId = "events_summary_cdm_name",
            label = "CDM name",
            choices = choices$events_summary_cdm_name,
            selected = selected$events_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "events_summary_cohort",
            label = "Cohort",
            choices = choices$events_summary_cohort,
            selected = selected$events_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "events_summary_treatment",
            label = "Treatment",
            choices = choices$events_summary_variable_level,
            selected = selected$events_summary_variable_level,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "events_summary_outcome",
            label = "Outcome",
            choices = choices$events_summary_outcome,
            selected = selected$events_summary_outcome,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Tidy",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::downloadButton(outputId = "events_summary_tidy_download", label = "Download csv")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = "events_summary_tidy_pivot_estimates",
                    label = "Pivot estimates",
                    value = TRUE
                  ),
                  position = "right"
                ),
                DT::DTOutput("events_summary_tidy") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Table",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "events_summary_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "events_summary_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              gt::gt_output("events_summary_table") |>
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    ),
    
    ## Followup summary
    bslib::nav_panel(
      title = "Followup summary",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_followup_summary", "update_message_followup_summary"),
          
          shinyWidgets::pickerInput(
            inputId = "followup_summary_cdm_name",
            label = "CDM name",
            choices = choices$followup_summary_cdm_name,
            selected = selected$followup_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "followup_summary_cohort",
            label = "Cohort",
            choices = choices$followup_summary_cohort,
            selected = selected$followup_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "followup_summary_treatment",
            label = "Treatment",
            choices = choices$followup_summary_variable_level,
            selected = selected$followup_summary_variable_level,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "followup_summary_outcome",
            label = "Outcome",
            choices = choices$followup_summary_outcome,
            selected = selected$followup_summary_outcome,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "followup_summary_reason",
            label = "Reason",
            choices = choices$followup_summary_reason,
            selected = selected$followup_summary_reason,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "followup_summary_estimate_name",
            label = "Estimate name",
            choices = choices$followup_summary_estimate_name,
            selected = selected$followup_summary_estimate_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Tidy",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::downloadButton(outputId = "followup_summary_tidy_download", label = "Download csv")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = "followup_summary_tidy_pivot_estimates",
                    label = "Pivot estimates",
                    value = TRUE
                  ),
                  position = "right"
                ),
                DT::DTOutput("followup_summary_tidy") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Table",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "followup_summary_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "followup_summary_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              gt::gt_output("followup_summary_table") |>
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    ),
    
    ## Survival summary
    bslib::nav_panel(
      title = "Survival summary",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_survival_summary", "update_message_survival_summary"),
          
          shinyWidgets::pickerInput(
            inputId = "survival_summary_cdm_name",
            label = "CDM name",
            choices = choices$survival_summary_cdm_name,
            selected = selected$survival_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "survival_summary_cohort",
            label = "Cohort",
            choices = choices$survival_summary_cohort,
            selected = selected$survival_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "survival_summary_treatment",
            label = "Treatment",
            choices = choices$survival_summary_variable_level,
            selected = selected$survival_summary_variable_level,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "survival_summary_outcome",
            label = "Outcome",
            choices = choices$survival_summary_outcome,
            selected = "death",
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "survival_summary_estimate_name",
            label = "Estimate name",
            choices = choices$survival_summary_estimate_name,
            selected = selected$survival_summary_estimate_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "survival_summary_time_limit",
            label = "Time Limit",
            choices = c("2 y", "5 y", "10 y", "Max"),
            selected = "Max",
            multiple = FALSE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Tidy",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::downloadButton(outputId = "survival_summary_tidy_download", label = "Download csv")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = "survival_summary_tidy_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  position = "right"
                ),
                DT::DTOutput("survival_summary_tidy") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Plot",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Survival curves"),
              shiny::plotOutput("survival_plot", height = 500) |>
                shinycssloaders::withSpinner()
            )
          ),
          bslib::nav_panel(
            title = "Log Minus Log Plot",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Proportionality of hazard ratios"),
              shiny::plotOutput("log_minus_log_plot", height = 500) |>
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    ),
    ## Empirical calibration (moved into Outcomes menu)
    bslib::nav_panel(
      title = "HR and Empirical calibration",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_calibration", "update_message_calibration"),
          
          shinyWidgets::pickerInput(
            inputId = "calibration_cdm_name",
            label = "CDM name",
            choices = choices$hr_summary_cdm_name,
            selected = selected$hr_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "calibration_cohort",
            label = "Cohort",
            choices = choices$hr_summary_cohort,
            selected = selected$hr_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "calibration_outcome",
            label = "Outcome",
            choices = choices$hr_summary_outcome,
            selected = "death",
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "calibration_nco",
            label = "NCO",
            choices = choices$nco_hr_summary_outcome,
            selected = selected$nco_hr_summary_outcome,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Hazard Ratio summary",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Hazard ratios before and after calibration"),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::pickerInput(
                    inputId = "hr_table_adjustment",
                    label = "Adjustment",
                    choices = c("None", "calibrated"),
                    selected = "None",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                bslib::card_header(
                  bslib::popover(
                    shiny::icon("download"),
                    shinyWidgets::pickerInput(
                      inputId = "calibrated_ci_table_format",
                      label = "Format",
                      choices = c("docx", "png", "pdf", "html"),
                      selected = "docx",
                      multiple = FALSE,
                      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                    ),
                    shiny::downloadButton(outputId = "calibrated_ci_table_download", label = "Download table")
                  ),
                  class = "text-end"
                ),
                gt::gt_output("calibrated_ci_table") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Forest Plot",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::numericInput(
                    inputId = "calibrated_forest_plot_width",
                    label = "Width",
                    value = 15
                  ),
                  shiny::numericInput(
                    inputId = "calibrated_forest_plot_height",
                    label = "Height",
                    value = 15
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "calibrated_forest_plot_units",
                    label = "Units",
                    choices = c("px", "cm", "inch"),
                    selected = "cm",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::numericInput(
                    inputId = "calibrated_forest_plot_dpi",
                    label = "DPI",
                    value = 300
                  ),
                  shiny::downloadButton(outputId = "calibrated_forest_plot_download", label = "Download plot")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::materialSwitch(
                    inputId = "calibrated_forest_plot_interactive",
                    label = "Interactive",
                    value = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "adjustment",
                    label = "Adjustment",
                    choices = c("None", "calibrated"),
                    selected = "None",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  position = "right"
                ),
                shiny::uiOutput("calibrated_forest_plot") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Empirical Calibration Plot",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::numericInput(
                    inputId = "calibration_plot_width",
                    label = "Width",
                    value = 15
                  ),
                  shiny::numericInput(
                    inputId = "calibration_plot_height",
                    label = "Height",
                    value = 15
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "calibration_plot_units",
                    label = "Units",
                    choices = c("px", "cm", "inch"),
                    selected = "cm",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::numericInput(
                    inputId = "calibration_plot_dpi",
                    label = "DPI",
                    value = 300
                  ),
                  shiny::downloadButton(outputId = "calibration_plot_download", label = "Download plot")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shinyWidgets::materialSwitch(
                    inputId = "calibration_plot_interactive",
                    label = "Interactive",
                    value = TRUE
                  ),
                  position = "right"
                ),
                shiny::uiOutput("calibration_plot") |>
                  shinycssloaders::withSpinner()
              )
            )
          )
        )
      )
    )
  ), ## end Outcomes nav_menu
  ## NCO menu
  bslib::nav_menu(
    title = "NCO",
    icon = shiny::icon("chart-bar"),
    
    ## NCO Events summary
    bslib::nav_panel(
      title = "Events summary",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_nco_events_summary", "update_message_nco_events_summary"),
          
          shinyWidgets::pickerInput(
            inputId = "nco_events_summary_cdm_name",
            label = "CDM name",
            choices = choices$nco_events_summary_cdm_name,
            selected = selected$nco_events_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_events_summary_cohort",
            label = "Cohort",
            choices = choices$nco_events_summary_cohort,
            selected = selected$nco_events_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_events_summary_treatment",
            label = "Treatment",
            choices = choices$nco_events_summary_variable_level,
            selected = selected$nco_events_summary_variable_level,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_events_summary_outcome",
            label = "Outcome",
            choices = choices$nco_events_summary_outcome,
            selected = selected$nco_events_summary_outcome,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Tidy",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shiny::downloadButton(outputId = "nco_events_summary_tidy_download", label = "Download csv")
                ),
                class = "text-end"
              ),
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = "nco_events_summary_tidy_pivot_estimates",
                    label = "Pivot estimates",
                    value = TRUE
                  ),
                  position = "right"
                ),
                DT::DTOutput("nco_events_summary_tidy") |>
                  shinycssloaders::withSpinner()
              )
            )
          ),
          bslib::nav_panel(
            title = "Table",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                bslib::popover(
                  shiny::icon("download"),
                  shinyWidgets::pickerInput(
                    inputId = "nco_events_summary_table_format",
                    label = "Format",
                    choices = c("docx", "png", "pdf", "html"),
                    selected = "docx",
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                  ),
                  shiny::downloadButton(outputId = "nco_events_summary_table_download", label = "Download table")
                ),
                class = "text-end"
              ),
              gt::gt_output("nco_events_summary_table") |>
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    ),
    
    ## NCO Hr summary (table)
    bslib::nav_panel(
      title = "Hr summary",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          update_button_card("update_nco_hr_summary", "update_message_nco_hr_summary"),
          
          shinyWidgets::pickerInput(
            inputId = "nco_hr_summary_cdm_name",
            label = "CDM name",
            choices = choices$nco_hr_summary_cdm_name,
            selected = selected$nco_hr_summary_cdm_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_hr_summary_cohort",
            label = "Cohort",
            choices = choices$nco_hr_summary_cohort,
            selected = selected$nco_hr_summary_cohort,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_hr_summary_outcome",
            label = "Outcome",
            choices = choices$nco_hr_summary_outcome,
            selected = selected$nco_hr_summary_outcome,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shinyWidgets::pickerInput(
            inputId = "nco_hr_summary_estimate_name",
            label = "Estimate name",
            choices = choices$nco_hr_summary_estimate_name,
            selected = selected$nco_hr_summary_estimate_name,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          position = "left",
          width = "300px"
        ),
        bslib::nav_panel(
          title = "Table",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shinyWidgets::pickerInput(
                  inputId = "nco_hr_summary_table_format",
                  label = "Format",
                  choices = c("docx", "png", "pdf", "html"),
                  selected = "docx",
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
                ),
                shiny::downloadButton(outputId = "nco_hr_summary_table_download", label = "Download table")
              ),
              class = "text-end"
            ),
            gt::gt_output("nco_hr_summary_table") |>
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  ), ## end NCO nav_menu
  
  ## Spacer and items on right
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(
        outputId = "download_raw",
        label = "Download raw data",
        icon = shiny::icon("download")
      )
    )
  ),
  bslib::nav_item(
    bslib::popover(
      shiny::icon("circle-info"),
      shiny::tags$img(
        src = "hds_logo.svg",
        class = "logo-img",
        alt = "Logo",
        height = "auto",
        width = "30%",
        style = "float:right"
      ),
      "This shiny app was generated with ",
      shiny::a(
        "OmopViewer",
        href = "https://github.com/OHDSI/OmopViewer",
        target = "_blank"
      ),
      shiny::strong("v0.4.0")
    )
  ),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
) ## end page_navbar
  
    