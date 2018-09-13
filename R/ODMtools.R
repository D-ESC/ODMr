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
        source(file.path("R/ODMtools", "tableUI.R"), local = TRUE)$value
      ),
      shiny::column(
        width = 4,
        source(file.path("R/ODMtools", "toolboxUI.R"), local = TRUE)$value,
        source(file.path("R/ODMtools", "consoleUI.R"), local = TRUE)$value
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
      values$ODMdata$index <- 1:nrow(values$ODMdata)
    })

    Data <- function(x = NULL) {
      key <- plotly::event_data("plotly_selected", source = "subset")$key
      if(length(key) == 0) {
        key = 1:nrow(values$ODMdata)
      }
      if (!is.null(x)) {
        values$ODMdata[values$ODMdata$index %in%
                         key & values$ODMdata$label == input$series, ] <-
          x[x$index %in% key & x$label == input$series, ]
      }
      result <- values$ODMdata[values$ODMdata$index %in% key &
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
        key = values$ODMdata$index,
        color = values$ODMdata$label,
        type = "scatter",
        mode = "markers",
        opacity = 0.8,
        source = "subset"
      ) %>%
        plotly::layout(legend = list(orientation = "h")) %>%
        plotly::toWebGL()
      P
    })

    source(file.path("R/ODMtools", "tableServer.R"), local = TRUE)$value
    source(file.path("R/ODMtools", "toolboxServer.R"), local = TRUE)$value
    source(file.path("R/ODMtools", "consoleServer.R"), local = TRUE)$value

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
