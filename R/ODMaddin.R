ODMtools <- function(...) {
  dataChoices <-
    ls(envir=.GlobalEnv)[unlist(lapply(lapply(
      ls(envir=.GlobalEnv),
      function(x){as.character(colnames(get(x)))}),
      function(x) "DataValue" %in% x && "LocalDateTime" %in% x))]

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("ODMtools"),
    miniUI::miniContentPanel(
      shiny::fillRow(flex = c(2,1),
        shiny::fillCol(
          rbokeh::rbokehOutput("rbokeh", width = "100%", height = "100%")),
        shiny::fillCol(flex = NA,
          shiny::selectInput("data", "Data:", choices = dataChoices),
          shiny::selectInput("ref", "Reference:", choices = dataChoices),
          shinyAce::aceEditor("code", mode="r", height = 100),
          shiny::actionButton("eval", "Evaluate"),
          shiny::br(),
          shiny::verbatimTextOutput("output"))
      )
    )
  )

  server <- function(input, output, session) {

    values <- shiny::reactiveValues(ODMdata = NULL, Refdata = NULL)

    shiny::observeEvent(input$data, {
      values$ODMdata <- get(input$data, envir=.GlobalEnv)
    })

    shiny::observeEvent(input$ref, {
      values$Refdata <- get(input$ref, envir=.GlobalEnv)
    })

    Data <- function (x = NULL, rows = input$selection_info + 1) {
      if(length(rows) == 0) rows <- 1:nrow(values$ODMdata)
      if(!is.null(x)) values$ODMdata[rows,] <- x
      values$ODMdata[rows,]
    }

    output$rbokeh <- rbokeh::renderRbokeh({
      P = rbokeh::figure(webgl = TRUE, lod_threshold = 0,
        width = 900, height = 700,
        tools = c("pan", "box_zoom", "resize", "reset")) %>%
        rbokeh::ly_points(x = LocalDateTime, y = DataValue, data = values$ODMdata,
          glyph = 1, size = 5, lname = "points", ...) %>%
        rbokeh::x_axis(label = "LocalDateTime") %>%
        rbokeh::y_axis(label = "DataValue") %>%
        rbokeh::tool_box_select(rbokeh::shiny_callback("selection_info"), "points") %>%
        rbokeh::tool_lasso_select(rbokeh::shiny_callback("selection_info"), "points")
      if(!is.null(values$Refdata)) ({
        P = P %>% rbokeh::ly_points(x = LocalDateTime, y = DataValue, data = values$Refdata,
          glyph = 1, size = 2, alpha = 0.5, lname = "reference", line_color = "grey", ...)
      })
      P
    })
    shiny::observe({
      shinyAce::updateAceEditor(session, "code",
        shiny::isolate(eval(parse(text=input$code))),
        mode="r")
    })
    output$output <- shiny::renderPrint({
      input$eval
      return(shiny::isolate(eval(parse(text=input$code))))
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = values$ODMdata)
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = "", width = 1024))
}

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
      DT::datatable(Catalog, selection = 'single')
    })

    shiny::observeEvent(input$done, {

      if (!is.null(input$table_rows_selected)) {
        Catalog <- ODMcatalog()
        rows <- input$table_rows_selected
        code <-
          paste(Catalog$SiteCode[rows], "_", Catalog$VariableCode[rows],
            " <- ODMr::ODMselect(ODM, ", rows, ")"
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
