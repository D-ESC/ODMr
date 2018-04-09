#' ODMDashboard
#'
#' Interactively visualise data from an ODM database. Download data as csv or
#' send to active R session.
#'
#' @export

ODMDashboard <- function() {
  ODM <- odbc::dbConnect(
    odbc::odbc(),
    dsn = "ODM",
    database = "OD",
    UID = "update",
    PWD = "update",
    Port = 1433
  )

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Observation Data"),
    shinydashboard::dashboardSidebar(
      ##choose how values are to be aggregated, default is day level for speed.#
      shiny::selectInput(
        inputId = "aggregate",
        label = "Aggregate By:",
        choices = c('none', 'hour', 'day', 'month'),
        selected = 'day',
        selectize = TRUE
      ),
      ##choose aggregate function to be applied, default is average.############
      shiny::conditionalPanel(
        condition = "input.aggregate != 'none'",
        shiny::selectInput(
          inputId = "fun",
          label = "Aggregate Function:",
          choices = c(
            'min' = 'min(DataValue, na.rm = TRUE)',
            'max' = 'max(DataValue, na.rm = TRUE)',
            'mean' = 'mean(DataValue, na.rm = TRUE)',
            'sum' = 'sum(DataValue, na.rm = TRUE)'
          ),
          selected = 'mean(DataValue, na.rm = TRUE)',
          selectize = TRUE
        )
      ),
      ##select a date range to return, smaller is quicker.######################
      shiny::checkboxInput("date_filter", "Date range:"),
      shiny::conditionalPanel(
        condition = "input.date_filter == true",
        shiny::dateRangeInput(
          "daterange",
          "Date range:",
          start = Sys.Date() - 365,
          end   = Sys.Date()
        )
      ),
      shiny::br(),
      ##button to download data as csv or send directly to R session.###########
      shiny::downloadButton("downloadData.csv", "Download", class = "butt"),
      shiny::tags$head(
        shiny::tags$style(
          ".butt{margin-left: 15px;} .skin-blue .sidebar a { color: #444; }"
        )
      ),
      shiny::br(),
      shiny::actionButton("done", "Send to R", width = '104px')
    ),
    ##map sites, plot selected data, print summary stats########################
    shinydashboard::dashboardBody(
      shinydashboard::tabBox(
        width = 12,
        height = '475px',
        shiny::tabPanel("Map", leaflet::leafletOutput("mymap")),
        shiny::tabPanel("Plot", dygraphs::dygraphOutput("plot")),
        shiny::tabPanel("Summary", shiny::tableOutput("stats"))
      ),
      DT::dataTableOutput("Dtbl")
    )
  )

  server <- function(input, output) {
    Sites <- ODM %>% dplyr::tbl("Sites") %>%
      dplyr::collect()

    output$mymap <- leaflet::renderLeaflet({
      pal = leaflet::colorFactor(RColorBrewer::brewer.pal(length(unique(
        Sites$SiteType
      )), "Set1"),
      Sites$SiteType)
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          lng = Sites$Longitude,
          lat = Sites$Latitude,
          popup = Sites$SiteName,
          color = pal(Sites$SiteType)
        ) %>%
        leaflet::addLegend("bottomright",
                           pal = pal,
                           values = Sites$SiteType)
    })

    in_bounding_box <- function(data, bounds) {
      data %>%
        dplyr::filter(
          Latitude > bounds$south &
            Latitude < bounds$north &
            Longitude < bounds$east & Longitude > bounds$west
        )
    }

    click <- shiny::reactiveValues(clickedMarker = NULL)

    shiny::observeEvent(input$mymap_marker_click,
                        {
                          click$clickedMarker <- input$mymap_marker_click
                        })

    shiny::observeEvent(input$mymap_click,
                        {
                          click$clickedMarker <- NULL
                        })

    data_map <- shiny::reactive({
      if (is.null(input$mymap_bounds)) {
        result <- Sites
      } else {
        bounds <- input$mymap_bounds
        result <- in_bounding_box(Sites, bounds)
        result
      }
      if (is.null(click$clickedMarker)) {
        result
      } else{
        result %>% dplyr::filter(Latitude == click$clickedMarker$lat,
                                 Longitude == click$clickedMarker$lng)
      }
    })

    output$Dtbl = DT::renderDataTable({
      shiny::req(data_map()$SiteCode)
      result <- ODM %>% dplyr::tbl("SeriesCatalog") %>%
        dplyr::select(
          SiteCode,
          VariableCode,
          MethodDescription,
          QCCode = QualityControlLevelCode,
          BeginDateTime,
          EndDateTime,
          Count = ValueCount
        ) %>%
        dplyr::filter(SiteCode %in% data_map()$SiteCode) %>%
        dplyr::collect()
      DT::datatable(result, filter = 'top', style = 'bootstrap')
    })

    data <- shiny::reactive({
      shiny::req(input$Dtbl_rows_selected)
      selection <- ODM %>% dplyr::tbl("SeriesCatalog") %>%
        dplyr::collect() %>%
        dplyr::filter(SiteCode %in% data_map()$SiteCode) %>%
        .[input$Dtbl_rows_selected, ]
      result <- ODM %>% dplyr::tbl("DataValues") %>%
        dplyr::filter(
          SiteID %in% selection$SiteID,
          VariableID %in% selection$VariableID,
          MethodID %in% selection$MethodID,
          QualityControlLevelID %in% selection$QualityControlLevelID
        )

      if (input$date_filter == TRUE) {
        result <- result %>%
          dplyr::filter(LocalDateTime > input$daterange[1] &
                          LocalDateTime < input$daterange[2])
      }
      if (input$aggregate == 'hour') {
        result <- result %>%
          dplyr::group_by(
            UTCOffset,
            SiteID,
            VariableID,
            MethodID,
            SourceID,
            QualityControlLevelID,
            LocalDateTime = DATEADD(hour, DATEDIFF(hour, 0, LocalDateTime), 0)
          )
      }
      if (input$aggregate == 'day') {
        result <- result %>%
          dplyr::group_by(
            UTCOffset,
            SiteID,
            VariableID,
            MethodID,
            SourceID,
            QualityControlLevelID,
            LocalDateTime = DATEADD(day, DATEDIFF(day, 0, LocalDateTime), 0)
          )
      }
      if (input$aggregate == 'month') {
        result <- result %>%
          dplyr::group_by(
            UTCOffset,
            SiteID,
            VariableID,
            MethodID,
            SourceID,
            QualityControlLevelID,
            LocalDateTime = DATEADD(month, DATEDIFF(month, 0, LocalDateTime), 0)
          )
      }
      if (input$aggregate == 'none') {
        result <- result %>%
          dplyr::select(LocalDateTime,
                          DataValue,
                          UTCOffset,
                          SiteID,
                          VariableID,
                          QualifierID,
                          MethodID,
                          SourceID,
                          QualityControlLevelID) %>%
          dplyr::collect()
      }
      if (input$aggregate != 'none') {
        result <-
          result %>%
          dplyr::summarise(DataValue = rlang::parse_expr(input$fun),
                           QualifierID = min(QualifierID, na.rm = TRUE)) %>%
          dplyr::mutate(QualityControlLevelID = NA) %>%
          dplyr::select(LocalDateTime,
                        DataValue,
                        UTCOffset,
                        SiteID,
                        VariableID,
                        QualifierID,
                        MethodID,
                        SourceID,
                        QualityControlLevelID) %>%
          dplyr::collect()
      }
      result
    })

    output$plot <- dygraphs::renderDygraph({
      result <- data()
      req(nrow(result) > 1)
      result <-
        ODM %>% dplyr::tbl("Sites") %>% dplyr::collect() %>%
        dplyr::select(SiteID, SiteCode) %>% dplyr::right_join(result)
      result <-
        ODM %>% dplyr::tbl("Variables") %>% dplyr::collect() %>%
        dplyr::select(VariableID, VariableCode) %>% dplyr::right_join(result)
      result %>%
        dplyr::select(LocalDateTime,
                      DataValue,
                      SiteCode,
                      VariableCode,
                      MethodID,
                      QualityControlLevelID) %>%
        tidyr::unite(Label,
                     SiteCode,
                     VariableCode,
                     MethodID,
                     QualityControlLevelID,
                     sep = "_") %>%
        tidyr::spread(Label, DataValue) %>%
        xts::xts(x = .[,-1],
                 order.by = .$LocalDateTime) %>%
        dygraphs::dygraph() %>% dygraphs::dyRoller(rollPeriod = 1)
    })

    output$stats <- shiny::renderTable({
      data() %>% dplyr::rename("QCLevelID" = "QualityControlLevelID") %>%
        dplyr::summarise(
          n = length(DataValue),
          mean = mean(DataValue, na.rm = TRUE),
          std.dev. = sd(DataValue, na.rm = TRUE),
          min = min(DataValue, na.rm = TRUE),
          p.05 = quantile(DataValue, 0.05, na.rm = TRUE),
          p.25 = quantile(DataValue, 0.25, na.rm = TRUE),
          p.50 = quantile(DataValue, 0.50, na.rm = TRUE),
          p.75 = quantile(DataValue, 0.75, na.rm = TRUE),
          p.95 = quantile(DataValue, 0.95, na.rm = TRUE),
          max = max(DataValue, na.rm = TRUE)
        )
    })

    output$downloadData.csv <- shiny::downloadHandler(
      filename = function() {
        paste("ODM_", gsub(":", "-", Sys.time()), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
    shiny::observeEvent(input$done, {
      ODMdata <<- data()
      invisible(shiny::stopApp())
    })
  }
  shiny::shinyApp(ui, server,  options = list(launch.browser = TRUE))
}
