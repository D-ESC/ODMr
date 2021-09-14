###############################################################################
Seriescatalog_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::dateRangeInput(
            ns("daterange"),
            "Collect Data From:",
            start = "1900-01-01",
            end   = Sys.Date()
          )

        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            inputId = ns("aggregate"),
            label = "Aggregate By:",
            choices = c(
              "none" = "none",
              "hour" = "hour",
              "day" = "day",
              "month" = "month"
            ),
            selected = "day",
            selectize = TRUE
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            inputId = ns("fun"),
            label = "Function:",
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
        shiny::column(
          width = 3,
          shiny::tags$head(shiny::tags$style(
            shiny::HTML(
              '#getData-getdata{margin-top: 25px; background-color:orange}'
            )
          )),
          shiny::actionButton(ns("getdata"), "Get Data")
        )
      ),
      DT::dataTableOutput(ns("Dtbl"))
    )
  }

###############################################################################
series_catalog_server <-
  function(id, connection) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
    ###########################################################################
    values <- shiny::reactiveValues(meta = NULL, ODMData = NULL)
    ###########################################################################
    catalog <-
      connection %>% dplyr::tbl("Seriescatalog") %>% dplyr::collect()
    ###########################################################################
    output$Dtbl <- DT::renderDataTable({
      catalog %>%
        dplyr::select(
          SiteCode,
          VariableCode,
          MethodDescription,
          QCCode = QualityControlLevelCode,
          BeginDateTime,
          EndDateTime,
          Count = ValueCount
        ) %>%
        DT::datatable(filter = "top", style = "bootstrap")
    })
    ###########################################################################
    shiny::observeEvent(input$getdata, {
      shiny::req(input$Dtbl_rows_selected)
      shinyjs::disable("getdata")
      shiny::withProgress(message = "Loading data...", value = 1, {
        selection <- catalog %>%
          dplyr::select(
            SiteID,
            SiteCode,
            VariableID,
            VariableCode,
            MethodID,
            MethodDescription,
            SourceID,
            Organization,
            QualityControlLevelID
          ) %>%
          .[input$Dtbl_rows_selected, ]
        for (i in seq_along(selection$SiteID)) {
          df <- odm_read(site_id = selection$SiteID[i],
                         variable_id = selection$VariableID[i],
                         method_id = selection$MethodID[i],
                         level_id = selection$QualityControlLevelID[i],
                         source_id = selection$SourceID[i],
                         start_date = input$daterange[1],
                         end_date = input$daterange[2],
                         aggregate_by = input$aggregate,
                         FUN = input$fun,
                         channel = connection,
                         n = 2000000
          )
          if (i == 1) {
            values$ODMdata = df
          } else {
            values$ODMdata = dplyr::bind_rows(values$ODMdata, df)
          }
        }

        values$meta <- selection %>%
          tidyr::unite(label,
                       SiteID,
                       VariableID,
                       MethodID,
                       QualityControlLevelID,
                       SourceID,
                       remove = FALSE) %>%
          dplyr::mutate(label = paste0("TS", label))
        values$ODMdata <- values$ODMdata %>%
          dplyr::mutate(index = 1:nrow(values$ODMdata)) %>%
          tidyr::unite(label,
                       SiteID,
                       VariableID,
                       MethodID,
                       QualityControlLevelID,
                       SourceID,
                       remove = FALSE) %>%
          dplyr::mutate(label = paste0("TS", label), edited = NA)
      })
      shinyjs::enable("getdata")
    })
    ###########################################################################
    return(values)
      })
  }
