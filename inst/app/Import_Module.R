###############################################################################
import_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::fileInput(
            ns("dataset"),
            "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        shiny::column(
          width = 6,
          shiny::tags$head(shiny::tags$style(
            shiny::HTML(
              "#getImport-update {margin-top: 25px; background-color:orange}"
            )
          )),
          shiny::actionButton(ns("update"), "Import to Tools"),
          shiny::tags$head(shiny::tags$style(
            shiny::HTML("#getImport-errors{color: red}")
          )),
          shiny::textOutput(ns("errors"))
        )
      ),
      shiny::fluidRow(
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
        ),
        shiny::column(
          width = 4,
          shiny::selectInput(
            ns("select_siteid"),
            "Select site ID column/table",
            NULL,
            multiple = FALSE
          ),
          shiny::selectInput(
            ns("select_variableid"),
            "Select variable ID column/table",
            NULL,
            multiple = FALSE
          ),
          shiny::selectInput(
            ns("select_qualifierid"),
            "Select qualifier column/default",
            NULL,
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 4,
          shiny::selectInput(
            ns("select_methodid"),
            "Select method ID column/table",
            NULL,
            multiple = FALSE
          ),
          shiny::selectInput(
            ns("select_sourceid"),
            "Select source ID column/table",
            NULL,
            multiple = FALSE
          ),
          shiny::selectInput(
            ns("select_levelid"),
            "Select level ID column/table",
            NULL,
            multiple = FALSE
          )
        )
      ),
      shiny::br(),
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("upload_table"))),
      shiny::br(),
      shiny::tabsetPanel(
        shiny::tabPanel("Sites", shinycssloaders::withSpinner(DT::dataTableOutput(ns("sites")))),
        shiny::tabPanel("Variables", shinycssloaders::withSpinner(DT::dataTableOutput(ns("variables")))),
        shiny::tabPanel("Methods", shinycssloaders::withSpinner(DT::dataTableOutput(ns("methods")))),
        shiny::tabPanel("Qualifiers", shinycssloaders::withSpinner(DT::dataTableOutput(ns("qualifiers")))),
        shiny::tabPanel("Sources", shinycssloaders::withSpinner(DT::dataTableOutput(ns("sources")))),
        shiny::tabPanel("QC Levels", shinycssloaders::withSpinner(DT::dataTableOutput(ns("QClevel"))))
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
        read.csv(input$dataset$datapath)
      },
      error = function(e) {
        output$errors <- shiny::renderText({
          paste(e)
        })
      }
      )
    })
    ###########################################################################
    shiny::observeEvent(upload_data(), {
      shiny::updateSelectInput(session, "select_localdatetime",
                               choices = colnames(upload_data()),
                               selected = agrep("localdatetime", colnames(upload_data()), 
                                                ignore.case = TRUE, value = TRUE)[1])
      shiny::updateSelectInput(session, "select_datavalues",
                               choices = colnames(upload_data()), 
                               selected = agrep("datavalue", colnames(upload_data()), 
                                                ignore.case = TRUE, value = TRUE)[1]) 
      shiny::updateSelectInput(session, "select_siteid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("siteid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table", 
                                                 agrep("siteid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1])
      )
      shiny::updateSelectInput(session, "select_variableid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("variableid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table",
                                                 agrep("variableid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1])
      )
      shiny::updateSelectInput(session, "select_qualifierid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("qualifierid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table",
                                                 agrep("qualifierid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1])
      )
      shiny::updateSelectInput(session, "select_methodid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("methodid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table",
                                                 agrep("methodid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1])
      )
      shiny::updateSelectInput(session, "select_sourceid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("sourceid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table",
                                                 agrep("sourceid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1])
      )
      shiny::updateSelectInput(session, "select_levelid",
                               choices = c("Select from table", colnames(upload_data())),
                               selected = ifelse(length(agrep("qualitycontrollevelid", colnames(upload_data()), 
                                                              ignore.case = TRUE, value = TRUE)) == 0,
                                                 "Select from table",
                                                 agrep("qualitycontrollevelid", colnames(upload_data()), 
                                                       ignore.case = TRUE, value = TRUE)[1]) 
      )
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
      DT::renderDT({
        odm_read_tbl(odm_tbl = "Sites", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(pageLength = 5,
                       lengthMenu = c(5, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$variables <-
      DT::renderDT({
        odm_read_tbl(odm_tbl = "Variables", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(pageLength = 5,
                       lengthMenu = c(5, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$methods  <-
      DT::renderDT({
        odm_read_tbl(odm_tbl = "Methods", channel = connection) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(pageLength = 5,
                       lengthMenu = c(5, 10, 15, 20)),
        selection = "single"
      )
    ###########################################################################
    output$qualifiers  <-
      DT::renderDT({
        odm_read_tbl(odm_tbl = "Qualifiers", channel = connection) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          selection = "single"
        )
      )
    ###########################################################################
    output$sources  <-
      DT::renderDT({
        odm_read_tbl(odm_tbl = "Sources", channel = connection) %>%
          dplyr::select(1:3) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          selection = "single"
        )
      )
    ###########################################################################
    output$QClevel  <-
      DT::renderDT({
        odm_read_tbl(odm_tbl = "QualityControlLevels", channel = connection) %>%
          dplyr::select(-QualityControlLevelCode) %>%
          dplyr::mutate_all(stringr::str_trunc, width = 128)},
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          selection = "single"
        )
      )
    ###########################################################################
    shiny::observe({
      req(upload_data())
      proxy %>% DT::selectColumns(as.character(which(
        names(upload_data()) %in%
          c(input$select_localdatetime, input$select_datavalues, input$select_siteid,
            input$select_variableid, input$select_qualifierid, input$select_methodid,
            input$select_levelid, input$select_sourceid)
      )))
    })
    ###########################################################################
    shiny::observeEvent(
      input$update, {
        tryCatch({
          data_to_import <- upload_data()
          data$ODMdata <- ODMr::odm_data(
            date_time = anytime::anytime(data_to_import[, input$select_localdatetime]),
            data_value = data_to_import[, input$select_datavalues],
            utc_offset = as.numeric(input$select_utcoffset),
            site_id = if(input$select_siteid == "Select from table") {
              odm_read_tbl(odm_tbl = "Sites", channel = connection)[input$sites_rows_selected, "SiteID"]
            } else {data_to_import[, input$select_siteid]},
            variable_id = if(input$select_variableid == "Select from table") {
              odm_read_tbl(odm_tbl = "Variables", channel = connection)[input$variables_rows_selected, "VariableID"]
            } else {data_to_import[, input$select_variableid]},
            qualifier_id = if(input$select_qualifierid == "Select from table") {
              odm_read_tbl(odm_tbl = "Qualifiers", channel = connection)[input$qualifiers_rows_selected, "QualifierID"]
            } else {data_to_import[, input$select_qualifierid]},
            method_id = if(input$select_methodid == "Select from table") {
              odm_read_tbl(odm_tbl = "Methods", channel = connection)[input$methods_rows_selected, "MethodID"]
            } else {data_to_import[, input$select_methodid]},
            source_id = if(input$select_sourceid == "Select from table") {
              odm_read_tbl(odm_tbl = "Sources", channel = connection)[input$sources_rows_selected, "SourceID"]
            } else {data_to_import[, input$select_sourceid]},
            level_id = if(input$select_levelid == "Select from table") {
              odm_read_tbl(odm_tbl = "QualityControlLevels", channel = connection)[input$QClevel_rows_selected, "QualityControlLevelID"]
            } else {data_to_import[, input$select_levelid]}
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
            ODMr:::odm_summary(data$ODMdata, channel = connection) %>%
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
      }
    )
  }
