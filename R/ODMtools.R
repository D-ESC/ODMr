#' ODMtools
#'
#' Interactively visualise data from an ODM database. R code can be executed and
#' data manipulated using the embedded Ace text editor.
#'
#' @export

ODMtools <- function() {
  dataChoices <-
    ls(envir = .GlobalEnv)[unlist(lapply
    (
      lapply
      (
        ls(envir = .GlobalEnv),
        function(x) {
          as.character(colnames(get(x)))
        }
      ),
      function(x)
        "DataValue" %in% x &&
          "LocalDateTime" %in% x
    ))]

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "ODM TOOLS"),
    shinydashboard::dashboardSidebar(
      shiny::selectInput(
        "data",
        "Data:",
        choices = c("Select data" = "", dataChoices),
        selected = NULL
      ),
      shiny::uiOutput("choose_series"),
      shiny::radioButtons(
        "mode",
        "Plot Type:",
        choices = list("point" = "markers", "line" = "lines"),
        inline = TRUE
      ),
      shiny::br(),
      shiny::downloadButton("downloadData.csv", "Download", class = "butt"),
      shiny::tags$head(
        shiny::tags$style(
          ".butt{margin-left: 15px;} .skin-blue .sidebar a { color: #444; }"
        )
      ),
      shiny::br(),
      shiny::actionButton("done", "Send to R", width = "104px")
    ),
    shinydashboard::dashboardBody(shiny::fluidRow(
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          collapsible = TRUE,
          plotly::plotlyOutput("plot1")
        ),
        shinydashboard::box(
          width = NULL,
          collapsible = TRUE,
          DT::dataTableOutput("DTable")
        )
      ),
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          collapsible = TRUE,
          shiny::selectInput(
            "action",
            label = "Select action",
            choices = list(
              "Shift Values" = "shift_values",
              "Set Values" = "set_values",
              "Drift Correction" = "drift_values",
              "Delete Values" = "delete_values",
              "Infill Missing" = "infill_values",
              "Set Flag" = "flag_values"
            ),
            selected = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'shift_values'",
            shiny::numericInput("shift", label = "Shift values", value = NULL)
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'set_values'",
            shiny::numericInput("set", label = "Set values", value = NULL)
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'drift_values'",
            shiny::splitLayout(
              shiny::numericInput("drift1", label = "Start value", value = 1),
              shiny::numericInput("drift2", label = "End value", value = 1)
            )
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'delete_values'",
            shiny::radioButtons(
              "delete",
              label = "Delete?",
              choices = list("True" = "TRUE", "False" = "FALSE"),
              selected = "FALSE",
              inline = TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'flag_values'",
            shiny::selectInput(
              "flag",
              label = "flag",
              choices = list(
                "missing" = 101,
                "bad" = 102,
                "estimated" = 105,
                "adjusted" = 106,
                "good" = 107
              ),
              selected = NULL
            )
          ),
          shiny::conditionalPanel(
            condition = "input.action == 'infill_values'",
            shiny::radioButtons(
              "interpolation",
              label = "interpolation",
              choices = list("linear" = "approx",
                             "spline" = "spline",
                             "last observation" = "locf"),
              selected = "linear",
              inline = TRUE
            ),
            shiny::numericInput("max_gap", label = "Max gap", value = Inf)
          ),
          shiny::actionButton("edit", "Do")
        ),
        shinydashboard::box(
          width = NULL,
          collapsible = TRUE,
          shinyAce::aceEditor("code", mode = "r", height = 200),
          shiny::actionButton("eval", "Evaluate"),
          shiny::br(),
          shiny::br(),
          shiny::verbatimTextOutput("output")
        )
      )
    ))
  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(ODMdata = NULL, Refdata = NULL)

    shiny::observeEvent(input$data, {
      shiny::req(input$data)
      values$ODMdata <- get(input$data, envir = .GlobalEnv)
      values$ODMdata <-
        values$ODMdata %>%
        dplyr::arrange(LocalDateTime) %>%
        tidyr::unite(
          label,
          SiteID,
          VariableID,
          MethodID,
          QualityControlLevelID
        )
      values$series <- unique(values$ODMdata$label)
      values$ODMdata$LocalDateTime <-
        lubridate::force_tz(values$ODMdata$LocalDateTime, "UTC")
    })

    Data <- function(x = NULL, insert = FALSE) {
      LocalDate <- as.POSIXct(plotly::event_data("plotly_selected", source = "subset")$x / 1000,
        origin = "1970-01-01", tz = "UTC"
      )
      if (length(LocalDate) == 0) {
        LocalDate <- values$ODMdata$LocalDateTime
      }
      if (!is.null(x)) {
        if (insert == FALSE) {
          values$ODMdata[values$ODMdata$LocalDateTime %in%
                           LocalDate & values$ODMdata$label == input$series, ] <-
            x[x$LocalDateTime %in% LocalDate & x$label == input$series, ]
        } else {
          values$ODMdata <- dplyr::anti_join(x, values$ODMdata, by = c("LocalDateTime", "label")) %>%
            dplyr::bind_rows(values$ODMdata) %>% dplyr::arrange(label, LocalDateTime)
        }
      }
      result <- values$ODMdata[values$ODMdata$LocalDateTime %in% LocalDate &
        values$ODMdata$label == input$series, ]
      result
    }

    output$choose_series <- shiny::renderUI({
      shiny::selectInput("series", "Active Series:", choices = values$series)
    })

    output$plot1 <- plotly::renderPlotly({
      shiny::req(values$ODMdata)
      P <- plotly::plot_ly(
        x = values$ODMdata$LocalDateTime,
        y = values$ODMdata$DataValue,
        color = values$ODMdata$label,
        type = "scatter",
        mode = input$mode,
        opacity = 0.6,
        source = "subset"
      ) %>%
        plotly::layout(legend = list(orientation = "h")) %>%
        plotly::toWebGL()
      P
    })

    output$DTable <- DT::renderDataTable({
      shiny::req(plotly::event_data("plotly_selected", source = "subset"))
      dt <-
        DT::datatable(Data(), style = "bootstrap", editable = TRUE)
    })

    shiny::observeEvent(input$DTable_cell_edit, {
      info <- input$DTable_cell_edit
      i <- info$row
      j <- info$col
      v <- info$value
      d <- Data()
      d[i, j] <- DT::coerceValue(v, Data()[[i, j]])
      d %>% Data()
      # replaceData(proxy2, d2, resetPaging = FALSE)  # important
    })

    shiny::observeEvent(input$edit, {
      if (input$action == "shift_values") {
        Data() %>% dplyr::mutate(
          DataValue = DataValue + input$shift,
          QualifierID = 106
        ) %>% Data()
      }
      if (input$action == "set_values") {
        Data() %>% dplyr::mutate(DataValue = input$set, QualifierID = 105) %>% Data()
      }
      if (input$action == "drift_values") {
        len <- length(Data()$DataValue)
        drift <- (input$drift2 - input$drift1) / len * 1:len
        Data() %>% dplyr::mutate(
          DataValue = DataValue + drift,
          QualifierID = 106
        ) %>% Data()
      }
      if (input$action == "delete_values") {
        Data() %>% dplyr::mutate(DataValue = NA, QualifierID = 102) %>% Data()
      }
      if (input$action == "flag_values") {
        Data() %>% dplyr::mutate(QualifierID = as.integer(input$flag)) %>% Data()
      }
      if (input$action == "infill_values") {
        Data() %>% tidyr::complete(LocalDateTime = seq(
            min(LocalDateTime), max(LocalDateTime),
            by = as.double(LocalDateTime[2] - LocalDateTime[1], units = "secs"))) %>%
            tidyr::fill(names(Data()[1,unlist(purrr::map(Data(), function(x) length(unique(x)))) == 1])) %>%
          Data(insert = TRUE)
        which_na <- is.na(Data()$DataValue)
        Data() %>% dplyr::arrange(LocalDateTime) %>%
          dplyr::mutate(DataValue = if (input$interpolation == 'approx') {
            zoo::na.approx(DataValue, maxgap = input$max_gap, na.rm = FALSE)
          } else if (input$interpolation == 'spline') {
            zoo::na.spline(DataValue, maxgap = input$max_gap, na.rm = FALSE)
          } else if (input$interpolation == 'locf') {
            zoo::na.locf(DataValue, maxgap = input$max_gap, na.rm = FALSE)
          }) %>% Data()
        Data() %>%
          dplyr::mutate(QualifierID = ifelse(is.na(Data()$DataValue) == which_na,
                        QualifierID, 105)) %>% Data()
      }
    })

    shiny::observe({
      shinyAce::updateAceEditor(session, "code",
        shiny::isolate(eval(parse(text = input$code))),
        mode = "r"
      )
    })

    output$output <- shiny::renderPrint({
      input$eval
      return(shiny::isolate(eval(
        parse(text = stringr::str_replace_all(input$code, "[\r]", ""))
      )))
    })

    output$downloadData.csv <- shiny::downloadHandler(
      filename = function() {
        paste("ODM_", gsub(":", "-", Sys.time()), ".csv", sep = "")
      },
      content = function(file) {
        utils::write.csv(values$ODMdata %>%
          tidyr::separate(
            label,
            c(
              "SiteID",
              "VariableID",
              "MethodID",
              "QualityControlLevelID"
            )
          ),
        file,
        row.names = FALSE
        )
      }
    )
    shiny::observeEvent(input$done, {
      Dat <- values$ODMdata %>% tidyr::separate(
        label,
        c(
          "SiteID",
          "VariableID",
          "MethodID",
          "QualityControlLevelID"
        )
      )
      ODMdata <<- Dat
      invisible(shiny::stopApp())
    })
  }
  shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
}
