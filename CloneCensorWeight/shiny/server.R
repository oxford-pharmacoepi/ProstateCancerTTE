
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
  output$codelists <- renderReactable({
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

}
