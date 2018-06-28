#' ODMDashboard
#'
#' Interactively visualise data from an ODM database. Download data as csv or
#' send to active R session.
#'
#' @export

ODMDashboard <- function() {
  ODM <-  pool::dbPool(
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
            'min' = 'min',
            'max' = 'max',
            'mean' = 'mean',
            'sum' = 'sum'
          ),
          selected = 'mean',
          selectize = TRUE
        )
      ),
      ##select a date range to return, smaller is quicker.######################
      shiny::dateRangeInput(
          "daterange",
          "Date range:",
          start = "1970-01-01",
          end   = Sys.Date()
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
        .[input$Dtbl_rows_selected,]

      for(i in seq_along(selection$SiteID)) {
        df <- ODMgetData(SiteID = selection$SiteID[i],
                               VariableID = selection$VariableID[i],
                               MethodID = selection$MethodID[i],
                               QualityControlLevelID = selection$QualityControlLevelID[i],
                               startDate = input$daterange[1],
                               endDate = input$daterange[2],
                               AggregateBy = input$aggregate,
                               FUN = input$fun,
                               channel = ODM)
        if (i == 1) {
          result = df
        } else {
          result = dplyr::bind_rows(result, df)
        }
      }
      return(result)
    })
    output$plot <- dygraphs::renderDygraph({
      result <- data()
      shiny::req(nrow(result) > 1)
      result <-
        ODM %>% dplyr::tbl("Sites") %>% dplyr::collect() %>%
        dplyr::select(SiteID, SiteCode) %>% dplyr::right_join(result)
      result <-
        ODM %>% dplyr::tbl("Variables") %>% dplyr::collect() %>%
        dplyr::select(VariableID, VariableCode) %>% dplyr::right_join(result)
      result %>%
        dplyr::select(
          LocalDateTime,
          DataValue,
          SiteCode,
          VariableCode,
          MethodID,
          QualityControlLevelID
        ) %>%
        tidyr::unite(Label,
                     SiteCode,
                     VariableCode,
                     MethodID,
                     QualityControlLevelID,
                     sep = "_") %>%
        tidyr::spread(Label, DataValue) %>%
        xts::xts(x = .[, -1],
                 order.by = .$LocalDateTime) %>%
        dygraphs::dygraph() %>% dygraphs::dyRoller(rollPeriod = 1)
    })

    output$stats <- shiny::renderTable({
      data() %>% dplyr::rename("QCLevelID" = "QualityControlLevelID") %>%
        dplyr::summarise(
          n = length(DataValue),
          mean = mean(DataValue, na.rm = TRUE),
          std.dev. = stats::sd(DataValue, na.rm = TRUE),
          min = min(DataValue, na.rm = TRUE),
          p.05 = stats::quantile(DataValue, 0.05, na.rm = TRUE),
          p.25 = stats::quantile(DataValue, 0.25, na.rm = TRUE),
          p.50 = stats::quantile(DataValue, 0.50, na.rm = TRUE),
          p.75 = stats::quantile(DataValue, 0.75, na.rm = TRUE),
          p.95 = stats::quantile(DataValue, 0.95, na.rm = TRUE),
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
