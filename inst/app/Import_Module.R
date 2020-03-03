###############################################################################
import_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::fileInput(
            ns("dataset"),
            "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          shiny::checkboxInput(ns("header"), "Header", TRUE),
          shiny::radioButtons(
            ns("sep"),
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ",",
            inline = TRUE
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("file_uploadeded")),
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("select_localdatetime"),
              "Select date/time column",
              NULL,
              multiple = FALSE
            ),
            shiny::selectInput(
              ns("select_utcoffset"),
              "Select UTC offset",
              list(
                "Eastern Standard Time" = -5,
                "Atlantic Standard Time" = -4,
                "Central Standard Time" = -6,
                "Mountain Standard Time" = -7,
                "Pacific Standard Time" = -8
              ),
              multiple = FALSE
            ),
            shiny::selectInput(
              ns("select_datavalues"),
              "Select data value column",
              NULL,
              multiple = FALSE
            )
          )
        ),
        shiny::column(
          width = 4,
          shiny::tags$head(shiny::tags$style(
            shiny::HTML(
              "#getImport-update {margin-top: 25px; background-color:orange}"
            )
          )),
          conditionalPanel(
            condition = sprintf(
              "input['%s'] && input['%s'] != ''
                           && input['%s'] != '' && input['%s'] != ''
                           && input['%s'] != ''",
              ns("QClevel_rows_selected"),
              ns("sources_rows_selected"),
              ns("methods_rows_selected"),
              ns("variables_rows_selected"),
              ns("sites_rows_selected")
            ),
            shiny::actionButton(ns("update"), "Import to Tools"),
            shiny::tags$head(shiny::tags$style(
              shiny::HTML("#getImport-errors{color: red}")
            )),
            shiny::textOutput(ns("errors")),
          )
        )
      ),
      shiny::br(),
      shiny::conditionalPanel(
        condition = sprintf("output['%s'] == true", ns("file_uploadeded")),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("upload_table"))),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("sites"))),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("variables"))),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("methods"))),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("sources"))),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("QClevel")))
      )
    )
  }

###############################################################################
import_server <-
  function(input, output, session, connection, data) {
    ###########################################################################

    upload_data <- shiny::reactive({
      shiny::req(input$dataset)
      tryCatch({
        read.csv(input$dataset$datapath,
                 header = input$header,
                 sep = input$sep)
      },
      error = function(e) {
        output$errors <- shiny::renderText({
          paste(e)
        })
      }
      )
    })
    ###########################################################################
    output$file_uploaded <- reactive({
      return(!is.null(upload_data()))
    })
    ###########################################################################
    #outputOptions(output, "file_uploadeded", suspendWhenHidden = FALSE)
    ###########################################################################
    shiny::observeEvent(upload_data(), {
      shiny::updateSelectInput(session, "select_localdatetime",
                               choices = colnames(upload_data()))
      shiny::updateSelectInput(session, "select_datavalues",
                               choices = colnames(upload_data()))
    })
    ###########################################################################
    output$upload_table  <- DT::renderDT(
      upload_data(),
      selection = list(target = "column"),
      options = list(pageLength = 5,
                     lengthMenu = c(5, 10, 15, 20))
    )
    ###########################################################################
    proxy <- DT::dataTableProxy("upload_table")
    ###########################################################################
    output$sites  <-
      DT::renderDT(
        odm_read_tbl(odm_tbl = "Sites", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128),
        options = list(pageLength = 3,
                       lengthMenu = c(3, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$variables  <-
      DT::renderDT(
        odm_read_tbl(odm_tbl = "Variables", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128),
        options = list(pageLength = 3,
                       lengthMenu = c(3, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$methods  <-
      DT::renderDT(
        odm_read_tbl(odm_tbl = "Methods", channel = connection) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128),
        options = list(pageLength = 3,
                       lengthMenu = c(3, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$sources  <-
      DT::renderDT(
        odm_read_tbl(odm_tbl = "Sources", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128),
        options = list(
          pageLength = 3,
          lengthMenu = c(3, 10, 15, 20),
          selection = "single"
        )
      )
    ###########################################################################
    output$QClevel  <-
      DT::renderDT(
        odm_read_tbl(odm_tbl = "QualityControlLevelID", channel = connection) %>%
          dplyr::select(-QualityControlLevelCode) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128),
        options = list(
          pageLength = 3,
          lengthMenu = c(3, 10, 15, 20),
          selection = "single"
        )
      )
    ###########################################################################
    shiny::observeEvent(
      input$update, {
        tryCatch({
          data_to_import <- upload_data()
          data$ODMdata <- ODMr::odm_data(
            date_time = anytime::anytime(data_to_import[, input$select_localdatetime]),
            data_value = data_to_import[, input$select_datavalues],
            utc_offset = as.numeric(input$select_utcoffset),
            site_id =
              odm_read_tbl(odm_tbl = "Sites", channel = connection)[input$sites_rows_selected, "SiteID"],
            variable_id =
              odm_read_tbl(odm_tbl = "Variables", channel = connection)[input$variables_rows_selected, "VariableID"],
            method_id =
              odm_read_tbl(odm_tbl = "Methods", channel = connection)[input$methods_rows_selected, "MethodID"],
            source_id =
              odm_read_tbl(odm_tbl = "Sources", channel = connection)[input$sources_rows_selected, "SourceID"],
            level_id =
              odm_read_tbl(odm_tbl = "QualityControlLevelID", channel = connection)[input$QClevel_rows_selected, "QualityControlLevelID"]
          ) %>%
            dplyr::mutate(index = seq_len(length(DataValue))) %>%
            dplyr::mutate(import = TRUE) %>%
            tidyr::unite(label,
                         SiteID,
                         VariableID,
                         MethodID,
                         QualityControlLevelID,
                         SourceID,
                         remove = FALSE) %>%
            dplyr::mutate(label = paste0("TS", label))
          data$meta <-
            odm_summary(data$ODMdata, channel = connection) %>%
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
            tidyr::unite(label,
                         SiteID,
                         VariableID,
                         MethodID,
                         QualityControlLevelID,
                         SourceID,
                         remove = FALSE) %>%
            dplyr::mutate(label = paste0("TS", label))
        },
        error = function(e) {
          output$errors <- shiny::renderText({
            paste(e)
          })
        })
        ###########################################################################
        shiny::observe({
          proxy %>% DT::selectColumns(as.character(which(
            names(upload_data()) %in%
              c(input$select_localdatetime, input$select_datavalues)
          )))
        })
      }
    )
  }
