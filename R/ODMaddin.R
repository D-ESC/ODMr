ODMgetDataValuesAddin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("get ODM data"),
        miniUI::miniContentPanel(
          DT::dataTableOutput("table")
        )
      )

  server <- function(input, output, session) {

    ODMcatalog <- shiny::reactive({
      ODM <- shiny::req(ODM)
      RODBC::sqlFetch(channel = ODM, sqtable = "SeriesCatalog")
    })

    output$table <- DT::renderDataTable({
      Catalog <- ODMcatalog()
      Catalog <- Catalog[, c("SiteCode", "VariableCode", "MethodDescription",
        "QualityControlLevelCode", "BeginDateTime", "EndDateTime",
        "ValueCount")]
      colnames(Catalog) <- c("Site", "Variable", "Method", "QCLevel",
        "Start", "End", "Count")
      DT::datatable(Catalog, selection = 'single', filter = "top")
    })

    shiny::observeEvent(input$done, {

      if (!is.null(input$table_rows_selected)) {
        Catalog <- ODMcatalog()
        rows <- input$table_rows_selected
        code <-
          paste(Catalog$SiteCode[rows],
                "_", Catalog$VariableCode[rows],
                "_", Catalog$MethodID[rows],
                "_", Catalog$QualityControlLevelID[rows],
            " <- ODMr::ODMselect(ODM, SiteID = ", Catalog$SiteID[rows],
            ", VariableID = ", Catalog$VariableID[rows],
            ", MethodID = ", Catalog$MethodID[rows],
            ", QCLevelID = ", Catalog$QualityControlLevelID[rows],
            ")"
            , sep = "")
      }
      rstudioapi::sendToConsole(paste(
        tryCatch(code, error = function(e) NA)),
        execute = TRUE)
      invisible(shiny::stopApp())
    })

  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = "", width = 1024))
}

odbcconnectAddin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("odbcconnect"),
    miniUI::miniContentPanel(
      shiny::textInput("dsn", "Data Source Name"),
      shiny::textInput("uid", "User"),
      shiny::passwordInput("pwd", "Password"))
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$done, {
      if (nzchar(input$dsn) && nzchar(input$uid) && nzchar(input$pwd)) {
        code1 <-
          paste(input$dsn,
            " <- RODBC::odbcConnect('", input$dsn, "','", input$uid, "','",
            input$pwd, "')", sep = "")
      }
      rstudioapi::sendToConsole(paste(
        tryCatch(code1, error = function(e) NA)),
        execute = TRUE)
      invisible(shiny::stopApp())
    })

  }
  shiny::runGadget(ui, server)
}

