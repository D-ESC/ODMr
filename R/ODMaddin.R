getODMdataAddin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("get ODM data"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Parameters", icon = shiny::icon("sliders"),
        miniUI::miniContentPanel(
          shiny::fillRow(
            shiny::fillCol(flex = NA, width = "95%",
              shiny::textInput("dsn", "Data Source Name"),
              shiny::textInput("uid", "User"),
              shiny::textInput("pwd", "Password"),
              shiny::actionButton("connect", "Connect")),
            shiny::fillCol(flex = NA,
              shiny::tags$br(),
              shiny::verbatimTextOutput("pODM"))
          )
        )
      ),
      miniUI::miniTabPanel("Data", icon = shiny::icon("table"),
        miniUI::miniContentPanel(
          DT::dataTableOutput("table")
        )
      )
    )
  )

  server <- function(input, output, session) {

    ODMconnect <- shiny::eventReactive(input$connect, {
      if (nzchar(input$dsn) && nzchar(input$uid) && nzchar(input$pwd)) {
        ODM <- RODBC::odbcConnect(input$dsn, input$uid, input$pwd)
      } else {
        tryCatch(get("ODM"), error = function (e) return())
      }
    })

    ODMcatalog <- shiny::reactive({
      ODM <- shiny::req(ODMconnect())
      RODBC::sqlFetch(channel = ODM, sqtable = "SeriesCatalog")
    })

    output$pODM <- shiny::renderPrint(
      shiny::req(ODMconnect())
    )

    output$table <- DT::renderDataTable({
      ODM <- shiny::req(ODMconnect())
      Catalog <- ODMcatalog()
      Catalog <- Catalog[,c("SiteCode", "VariableCode", "MethodDescription",
        "QualityControlLevelCode", "BeginDateTime", "EndDateTime", "ValueCount")]
      colnames(Catalog) <- c("Site", "Variable", "Method", "QCLevel", "Start", "End", "Count")
      return(Catalog)
    })

    shiny::observeEvent(input$done, {
      if (nzchar(input$dsn) && nzchar(input$uid) && nzchar(input$pwd)) {
        code1 <-
          paste(input$dsn,
            " <- RODBC::odbcConnect('",input$dsn, "','", input$uid, "','", input$pwd, "')"
            , sep = "")
      }
      if (!is.null(input$table_rows_selected)) {
        Catalog <- ODMcatalog()
        rows <- input$table_rows_selected
        code2 <-
          paste(Catalog$SiteCode[rows],
            " <- ODMr::ODMselect(ODM, ",rows,")"
            , sep = "")
      }
      rstudioapi::sendToConsole(paste(
        tryCatch(code1, error = function(e) NA),
        tryCatch(code2, error = function(e) NA), sep = "; "),
        execute = TRUE)
      invisible(shiny::stopApp())
    })

  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = "", width = 1024))
}
