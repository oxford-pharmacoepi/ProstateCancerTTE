
server <- function(input, output, session) {
  # codelists ----
  observeEvent(input$select_codelist_type, {
    nms <- names(codelists[[input$select_codelist_type]])
    updatePickerInput(
      inputId = "select_codelist",
      choices = nms,
      selected = nms[1]
    )
  })
  output$codelist_definition <- renderReactable({
    req(input$select_codelist)
    type <- isolate(input$select_codelist_type)
    x <- codelists[[type]][[input$select_codelist]]
    coldef <- list(
      concept_id = colDef(name = "Concept ID", defaultSortOrder = "asc"),
      concept_name = colDef(name = "Concept Name", minWidth = 500),
      domain_id = colDef(name = "Domain ID"),
      vocabulary_id = colDef(name = "Vocabulary ID"),
      concept_class_id = colDef(name = "Concept Class ID", minWidth = 150),
      concept_code = colDef(name = "Concept Code", minWidth = 130)
    )
    reactable(data = x, filterable = TRUE, columns = coldef, defaultPageSize = 100)
  })
  output$download_codelist <- downloadHandler(
    filename = paste0(input$select_codelist, ".csv"),
    content = function(file) {
      x <- codelists[[input$select_codelist_type]][[input$select_codelist]]
      write_csv(x = x, file = file)
    }
  )
  output$codelist_code_use <- render_gt({
    results$code_use |>
      filterGroup(codelist_name == input$select_codelist) |>
      tableCodeUse(groupColumn = "codelist_name")
  })

  # cohorts ----
  output$ui_cohort_definition <- renderUI({
    paste0(results$cohort_definitions[input$select_cohort], collapse = "\n\n----\n\n") |>
      markdownToHTML(fragment.only = TRUE) |>
      HTML()
  })
  output$cohort_count <- render_gt({
    results$summarise_cohort_count |>
      filterGroup(cohort_name %in% input$select_cohort) |>
      filter(cdm_name %in% input$select_cdm_cohort) |>
      tableCohortCount(header = c("cdm_name"))
  })
  output$dwn_cohort_attrition <- downloadHandler(
    filename = "cohort_attrition.png",
    content = function(file) {
      res <- results$summarise_cohort_attrition |>
        filterGroup(cohort_name %in% input$select_cohort) |>
        filter(cdm_name %in% input$select_cdm_cohort)
      n <- length(unique(res$group_level))
      plotCohortAttrition(result = res, type = "DiagrammeR") |>
        render_graph() |>
        export_svg() |>
        charToRaw() |>
        rsvg_png(file = file, width = 2000 * n)
    }
  )
  output$cohort_attrition <- renderDiagrammeR({
    results$summarise_cohort_attrition |>
      filterGroup(cohort_name %in% input$select_cohort) |>
      filter(cdm_name %in% input$select_cdm_cohort) |>
      plotCohortAttrition()
  })

  # characteristics ----
  dataCohortCharacteristics <- reactive({
    results$summarise_characteristics |>
      filterGroup(cohort_name %in% input$select_cohort_char) |>
      filter(cdm_name %in% input$select_cdm_cohort_char)
  })
  output$cohort_characteristics <- render_gt({
    tableCharacteristics(
      result = dataCohortCharacteristics(),
      groupColumn = "variable_name",
      .options = list(decimals = c(numeric = 1, percentage = 1, integer = 0))
    )
  })
  output$plt_characteristics <- renderPlot({
    results$summarise_characteristics_plot |>
      filterGroup(cohort_name %in% input$select_cohort_char) |>
      filter(cdm_name %in% input$select_cdm_cohort_char) |>
      filter(variable_name == input$plt_char_variable) |>
      plotCharacteristics(plotType = "densityplot",
                          facet = input$plt_char_facet,
                          colour = input$plt_char_colour)
  })
  dataLsc <- reactive({
    results$summarise_large_scale_characteristics |>
      filterGroup(cohort_name %in% input$select_cohort_char) |>
      filter(cdm_name %in% input$select_cdm_cohort_char) |>
      filter(variable_level %in% input$lsc_window) |>
      filterSettings(table_name %in% input$lsc_table_name)
  })
  observe({
    compareBy <- input$lsc_compare_by
    if (compareBy == "window") {
      compareBy <- "variable_level"
      req(input$lsc_window)
      optsLsc <- unique(dataLsc()$variable_level)
    } else if (compareBy == "cohort_name") {
      optsLsc <- input$select_cohort_char
    } else if (compareBy == "cdm_name") {
      optsLsc <- input$select_cdm_cohort_char
    } else {
      optsLsc <- NULL
    }
    updatePickerInput(inputId = "lsc_smd_reference", choices = optsLsc, selected = optsLsc[1])
  })
  output$tbl_lsc_explore <- renderReactable({
    compareBy <- input$lsc_compare_by
    if (compareBy == "window") {
      compareBy <- "variable_level"
    } else if (compareBy == "no SMD") {
      compareBy <- NULL
    }
    tableLargeScaleCharacteristics(
      result = dataLsc(),
      compareBy = compareBy,
      smdReference = input$lsc_smd_reference
    )
  })
  output$tbl_lsc_top_concepts <- render_gt({
    tableTopLargeScaleCharacteristics(result = dataLsc(), topConcepts = input$lsc_top_concepts)
  })
  observeEvent(input$plt_lsc_colour, {
    col <- input$plt_lsc_colour
    if (col == "cohort_name") {
      opts <- input$select_cohort_char
    } else if (col == "cdm_name") {
      opts <- input$select_cohort_char
    } else if (col == "window") {
      opts <- input$lsc_window
    } else {
      opts <- NULL
    }
    updatePickerInput(inputId = "plt_lsc_reference", choices = opts, selected = opts[1])
  })
  output$plt_lsc_compared <- renderPlotly({
    if (input$plt_lsc_missings) {
      mis <- 0
    } else {
      mis <- NULL
    }
    facet <- input$plt_lsc_facet
    facet[facet == "window"] <- "variable_level"
    colour <- input$plt_lsc_colour
    colour[colour == "window"] <- "variable_level"
    plotComparedLargeScaleCharacteristics(
      result = dataLsc(),
      colour = colour,
      reference = input$plt_lsc_reference,
      facet = facet,
      missings = mis
    ) +
      aes(shape = table_name)
  })

  # diagnostics ----
  output$plt_subjects <- renderPlot({
    # get data
    if (input$weighted_sub) {
      subj <- "number_weighted_subjects"
    } else {
      subj <- "number_subjects"
    }
    x <- results$survival_summary |>
      filter(
        cdm_name %in% input$select_cdm_diag,
        outcome %in% input$select_out_diag
      ) |>
      select("cdm_name", "cohort_name", "outcome", "time", "Number subjects" = all_of(subj), "group_id")

    # edit colour
    if (length(input$colour_sub) > 0) {
      x <- x |>
        unite(col = "colour", all_of(input$colour_sub), sep = "; ")
    } else {
      x <- x |>
        mutate(colour = "all")
    }

    # create plot
    p <- x |>
      ggplot(mapping = aes(x = time, y = `Number subjects`, group = group_id, colour = colour)) +
      geom_step()

    # facet
    if (length(input$facet_sub) > 0) {
      p <- p +
        facet_wrap(facets = input$facet_sub)
    }

    m <- max(x[["Number subjects"]])

    p +
      ylim(0, m) +
      theme(
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
  })
  output$gt_events <- render_gt({
    if (input$weighted_events) {
      events <- "number_weighted_events"
      type <- "numeric"
    } else {
      events <- "number_events"
      type <- "integer"
    }
    results$events |>
      filter(
        cdm_name %in% input$select_cdm_diag,
        outcome %in% input$select_out_diag
      ) |>
      select("cdm_name", "cohort_name", "outcome", "n" = all_of(events)) |>
      group_by(cdm_name, cohort_name, outcome) |>
      summarise(estimate_value = sum(n), .groups = "drop") |>
      mutate(estimate_type = type, estimate_name = "events") |>
      formatEstimateValue() |>
      formatHeader(header = c("cdm_name", "cohort_name")) |>
      select(!c("estimate_name", "estimate_type")) |>
      formatTable(groupColumn = "outcome")
  })
  output$gt_followup <- render_gt({
    results$followup_summary |>
      filter(cdm_name %in% input$select_cdm_diag) |>
      filterStrata(outcome %in% input$select_out_diag) |>
      visOmopTable(
        estimateName = c("(%) min [q05; q25 - q50 - q75; q95] max" = "(<percentage>%) <min> [<q05>; <q25> - <median> - <q75>; <q95>] <max>"),
        header = c("cdm_name", "cohort_name"),
        groupColumn = "outcome",
        hide = c("variable_name", "variable_level"),
        .options = list(decimals = c(integer = 0, numeric = 1))
      )
  })
  output$tbl_coef <- renderReactable({
    results$coefficients |>
      filter(cdm_name %in% input$select_cdm_diag & outcome %in% input$select_out_diag) |>
      reactable(filterable = TRUE)
  })
  output$plt_ps <- renderPlot({
    p <- results$probabilities |>
      filter(cdm_name %in% input$select_cdm_diag,
             outcome %in% input$select_out_diag,
             time_start %in% input$plt_ps_times) |>
      ggplot(mapping = aes(x = prob_bin, y = freq, colour = prob_label, group = group_id)) +
      geom_line()
    opts <- c("cdm_name", "cohort_name", "outcome", "time_start")
    val <- c(input$plt_ps_cdm_name, input$plt_ps_cohort_name, input$plt_ps_outcome, input$plt_ps_time)
    x <- opts[!val]
    if (length(x) == 0) {
      x <- "."
    } else {
      x <- paste0(x, collapse = " + ")
    }
    y <- opts[val]
    if (length(y) == 0) {
      y <- "."
    } else {
      y <- paste0(y, collapse = " + ")
    }
    facets <- as.formula(paste0(x, " ~ ", y))
    p +
      facet_grid(facets) +
      theme(legend.position = "top")
  })
  output$plt_we <- renderPlot({
    p <- results$probabilities |>
      filter(cdm_name %in% input$select_cdm_diag,
             outcome %in% input$select_out_diag,
             time_start %in% input$plt_we_times,
             cohort_name == prob_label) |>
      mutate(prob_bin = 1 - prob_bin) |>
      rename("weight_bin" = "prob_bin", "weight_label" = "prob_label") |>
      ggplot(mapping = aes(x = weight_bin, y = freq, colour = weight_label, group = group_id)) +
      geom_line()
    opts <- c("cdm_name", "outcome", "time_start")
    val <- c(input$plt_we_cdm_name, input$plt_we_outcome, input$plt_we_time)
    x <- opts[!val]
    if (length(x) == 0) {
      x <- "."
    } else {
      x <- paste0(x, collapse = " + ")
    }
    y <- opts[val]
    if (length(y) == 0) {
      y <- "."
    } else {
      y <- paste0(y, collapse = " + ")
    }
    facets <- as.formula(paste0(x, " ~ ", y))
    p +
      facet_grid(facets) +
      theme(legend.position = "top")
  })

  # result ----
  output$plt_res_km <- renderPlot({
    p <- results$survival_summary |>
      filter(cdm_name %in% input$select_cdm_result,
             outcome %in% input$select_out_result) |>
      ggplot(aes(x = time, y = survival, ymin = lower_survival, ymax = upper_survival, colour = cohort_name, group = group_id, fill = cohort_name)) +
      geom_step() +
      geom_ribbon(alpha = 0.2, colour = NA)
    opts <- c("cdm_name", "outcome")
    val <- c(input$plt_km_cdm_name, input$plt_km_outcome)
    x <- opts[!val]
    if (length(x) == 0) {
      x <- "."
    } else {
      x <- paste0(x, collapse = " + ")
    }
    y <- opts[val]
    if (length(y) == 0) {
      y <- "."
    } else {
      y <- paste0(y, collapse = " + ")
    }
    facets <- as.formula(paste0(x, " ~ ", y))
    p +
      facet_grid(facets) +
      theme(legend.position = "top")
  })
  output$plt_result <- renderPlot({
    results$hr_summary |>
      filter(cdm_name %in% input$select_cdm_result,
             outcome %in% input$select_out_result,
             reference == input$plt_result_reference) |>
      ggplot(mapping = aes(x = hr, xmin = hr_lower, xmax = hr_upper, y = y, colour = comparator, shape = interval)) +
      geom_point(size = 2) +
      geom_errorbar(size = 0.75, width = 0) +
      facet_grid(outcome ~ cdm_name) +
      geom_vline(xintercept = 1) +
      coord_cartesian(xlim = c(0.1, 10)) +
      scale_x_log10(
        name = "Hazard ratio",
        breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10),
        labels = c("1/10", "1/4", "1/2", "1", "2", "4", "10")
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      ) +
      scale_colour_manual(values = c(
        surveillance = "#619CFF", prostatectomy = "#F8766D", radiotheraphy = "#00BA38"
      )) +
      geom_text(
        mapping = aes(x = x, y = y, label = label, hjust = 0),
        data = tibble(x = 0.1, y = c(-1, -3.5, -6) + 0.5, label = c("overall", "first year", "second year")),
        inherit.aes = FALSE,
        colour = "black"
      )
  })

}
