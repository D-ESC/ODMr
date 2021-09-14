server <- function(input, output, session) {
  ODMdata <- series_catalog_server("getData", connection = ODM)
  brushed <- Plot_server("getPlot", data = ODMdata)
  table_server("getTable", data = ODMdata,
    selected = brushed, active = activerow1)
  Stat_server("getStat", data = ODMdata,
    selected = brushed, active = activerow1)
  Toolbox_server("getToolbox", connection = ODM, data = ODMdata,
    selected = brushed, active = activerow1)
  console_server("getConsole", data = ODMdata,
                   selected = brushed, active = activerow1)
  import_server("getImport", connection = ODM, data = ODMdata)
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
      paste0("ODM_", gsub(":", "-", Sys.time()), ".csv.zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      tm <- gsub(":", "-", Sys.time())
      utils::write.csv(ODMdata$ODMdata %>% 
                         dplyr::select(SiteID, LocalDateTime, UTCOffset, DataValue,
                                       QualifierID, VariableID, MethodID, SourceID,
                                       QualityControlLevelID),
                       paste0(tmpdir, "\\", paste0("ODM_", tm, ".csv")),
                       row.names = FALSE)
      utils::write.csv(ODMdata$meta,
                       paste0(tmpdir, "\\", paste0("meta_", tm, ".csv")),
                       row.names = FALSE)
      zip::zipr(zipfile = file, files = c(paste0(tmpdir, "\\", paste0("ODM_", tm, ".csv")),
                                          paste0(tmpdir, "\\", paste0("meta_", tm, ".csv"))
                                          ))
    }, contentType = "application/zip"
  )
}
