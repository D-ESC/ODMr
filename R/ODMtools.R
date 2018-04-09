#' ODMtools
#'
#' Interactively visualise data from an ODM database. R code can be executed and
#' data manipulated using the embedded Ace text editor.
#'
#' @export

ODMtools <- function(...) {
  dataChoices <-
    ls(envir = .GlobalEnv)[unlist(lapply
                                  (lapply
                                    (ls(envir = .GlobalEnv),
                                      function(x) {
                                        as.character(colnames(get(x)))
                                      }),
                                    function(x)
                                      "DataValue" %in% x &&
                                      "LocalDateTime" %in% x))]

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "ODM TOOLS"),
    shinydashboard::dashboardSidebar(
      shiny::selectInput(
        "data",
        "Data:",
        choices = c('Select data' = '', dataChoices),
        selected = NULL
      ),
      shiny::uiOutput("choose_series"),
      shiny::radioButtons("mode", "Plot Type:",
                          choices = list("point" = "markers", "line" = "lines"),
                          inline = TRUE),
      shiny::br(),
      shiny::downloadButton("downloadData.csv", "Download", class = "butt"),
      shiny::tags$head(shiny::tags$style(".butt{margin-left: 15px;} .skin-blue .sidebar a { color: #444; }")),
      shiny::br(),
      shiny::actionButton("done", "Send to R", width = '104px')
    ),
    shinydashboard::dashboardBody(shiny::fluidRow(
      shiny::column(width = 8,
                    shinydashboard::box(width = NULL, collapsible = TRUE,
                                        plotly::plotlyOutput("plot1")),
                    shinydashboard::box(width = NULL, collapsible = TRUE,
                                        DT::dataTableOutput("DTable"))
      ),
      column(width = 4,
             shinydashboard::box(width = NULL, collapsible = TRUE,
                                 selectInput("action", label = "Select action",
                                             choices = list("shift" = "shift_values",
                                                            "set" = "set_values",
                                                            "drift" = "drift_values",
                                                            "delete" = "delete_values"),
                                             selected = NULL),
               shiny::conditionalPanel(condition = "input.action == 'shift_values'",
                 numericInput("shift", label = "Shift values", value = NULL)),
               shiny::conditionalPanel(condition = "input.action == 'set_values'",
                 numericInput("set", label = "Set values", value = NULL)),
               shiny::conditionalPanel(condition = "input.action == 'drift_values'",
                 shiny::splitLayout(
                   numericInput("drift1", label = "Start value", value = 1),
                   numericInput("drift2", label = "End value", value = 1))),
               shiny::conditionalPanel(condition = "input.action == 'delete_values'",
                                       radioButtons("delete", label = "Delete?",
                                                    choices = list("True" = "TRUE", "False" = "FALSE"),
                                                    selected = "FALSE")),
               actionButton("edit", "Do")),
             shinydashboard::box(
               width = NULL, collapsible = TRUE,
               shinyAce::aceEditor("code", mode = "r", height = 200),
               shiny::actionButton("eval", "Evaluate"),
               shiny::br(), shiny::br(),
               shiny::verbatimTextOutput("output"))
      )
    ))
  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(ODMdata = NULL, Refdata = NULL)

    shiny::observeEvent(input$data, {
      shiny::req(input$data)
      values$ODMdata <- get(input$data, envir = .GlobalEnv)
      values$ODMdata <- values$ODMdata %>% dplyr::arrange(LocalDateTime) %>%
        tidyr::unite(label, SiteID, VariableID, MethodID, QualityControlLevelID)
      values$series <- unique(values$ODMdata$label)
    })

    Data <- function (x = NULL,
                      rows = plotly::event_data("plotly_selected", source = "subset")$pointNumber + 1) {
      if (length(rows) == 0)
        rows <- 1:nrow(values$ODMdata)
      if (!is.null(x))
        result = values$ODMdata[1:nrow(values$ODMdata) %in% rows & values$ODMdata$label == input$series,] <- x
      result = values$ODMdata[1:nrow(values$ODMdata) %in% rows & values$ODMdata$label == input$series,]
      result
    }

    output$choose_series <- renderUI({
      selectInput("series", "Active Series:", choices = values$series)
    })

    output$plot1 <- plotly::renderPlotly({
      req(values$ODMdata)
      P = plotly::plot_ly(
        x = values$ODMdata$LocalDateTime,
        y = values$ODMdata$DataValue, color = values$ODMdata$label,
        type = "scatter", mode = input$mode, source = "subset") %>%
        plotly::toWebGL()
      P
    })

    output$DTable <- DT::renderDataTable({
      req(plotly::event_data("plotly_selected", source = "subset"))
      dt <- DT::datatable(Data(), style = "bootstrap", editable = TRUE)
    })

    observeEvent(input$DTable_cell_edit, {
      info = input$DTable_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      d = Data()
      d[i, j] <- DT::coerceValue(v, Data()[[i, j]])
      d %>% Data()
      #replaceData(proxy2, d2, resetPaging = FALSE)  # important
    })

    observeEvent(input$edit, {
      if (input$action == 'shift_values')
        Data() %>% dplyr::mutate(DataValue = DataValue + input$shift,
                                 QualifierID = 105) %>% Data()
      if (input$action == 'set_values')
        Data() %>% dplyr::mutate(DataValue = input$set, QualifierID = 105) %>% Data()
      if (input$action == 'drift_values') {
        len = length(Data()$DataValue)
        drift = (input$drift2 - input$drift1) / len * 1:len
        Data() %>% dplyr::mutate(DataValue = DataValue + drift,
                                 QualifierID = 105) %>% Data()}
      if (input$action == 'delete_values')
        Data() %>% dplyr::mutate(DataValue = NA, QualifierID = 102) %>% Data()
    })

    shiny::observe({
      shinyAce::updateAceEditor(session, "code",
                                shiny::isolate(eval(parse(text = input$code))),
                                mode = "r")
    })

    output$output <- shiny::renderPrint({
      input$eval
      return(shiny::isolate(eval(
        parse(
          text = stringr::str_replace_all(input$code, "[\r]" , "")))))
    })

    output$downloadData.csv <- downloadHandler(
      filename = function(){
        paste("ODM_",gsub(":","-",Sys.time()), ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$ODMdata %>%
                    tidyr::separate(label,
                                    c("SiteID", "VariableID", "MethodID", "QualityControlLevelID")),
                  file, row.names = FALSE)
      }
    )
    observeEvent(input$done, {
      Dat <- values$ODMdata %>% tidyr::separate(label,
                                                c("SiteID", "VariableID", "MethodID", "QualityControlLevelID"))
      ODMdata <<- Dat
      invisible(shiny::stopApp())
    })
  }
  shiny::shinyApp(ui, server,  options = list(launch.browser = TRUE))
}
