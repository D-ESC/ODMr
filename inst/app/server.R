server <- function(input, output, session) {
  ODMdata <- shiny::callModule(
    series_catalog_server, "getData", connection = ODM)
  brushed <- shiny::callModule(
    Plot_server, "getPlot", data = ODMdata)
  shiny::callModule(
    table_server, "getTable", data = ODMdata,
    selected = brushed, active = activerow1)
  shiny::callModule(
    Stat_server, "getStat", data = ODMdata,
    selected = brushed, active = activerow1)
  shiny::callModule(
    Toolbox_server, "getToolbox", connection = ODM, data = ODMdata,
    selected = brushed, active = activerow1)
  shiny::callModule(
    console_server, "getConsole", data = ODMdata,
    selected = brushed, active = activerow1)
  shiny::callModule(
    import_server, "getImport", connection = ODM, data = ODMdata)
  output$activeseries1 <- output$activeseries2 <-
    DT::renderDataTable(ODMdata$meta %>%
                          DT::datatable(style = 'bootstrap',
                                        selection = list(mode = 'single',
                                                         selected = 1),
                                        options = list(
                                          pageLength = 3,
                                          dom = 'tp')))
  activerow1 <- shiny::reactive({
    if(is.null(input$activeseries1_rows_selected)) {
    1} else {
      input$activeseries1_rows_selected
    }
  }
  )
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste("ODM_", gsub(":", "-", Sys.time()), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(ODMdata$ODMdata %>% dplyr::select(-index),
                       file,
                       row.names = FALSE
      )
    }
  )
}
