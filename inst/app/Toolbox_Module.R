###############################################################################
Toolbox_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::selectInput(
        ns("action"),
        label = "Select Action",
        choices = list(
          "Edit Values" = "apply_function",
          "Delete Values" = "delete_values",
          "Set Flag" = "flag_values",
          "Revert To Raw Values" = "revert_raw"
        ),
        selected = NULL
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'apply_function'"),
        shiny::textInput(ns("apply"), label = "Apply function", value = "DataValue = DataValue")
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'delete_values'"),
        shiny::radioButtons(
          ns("delete"),
          label = "Delete?",
          choices = list("True" = "TRUE", "False" = "FALSE"),
          selected = "FALSE",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'flag_values'"),
        shiny::selectInput(
          ns("flag"),
          label = "Flag",
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
        condition = paste0("input['", ns("action"), "'] == 'revert_raw'"),
        shiny::radioButtons(
          ns("raw"),
          label = "Revert to raw?",
          choices = list("True" = "TRUE", "False" = "FALSE"),
          selected = "FALSE",
          inline = TRUE
        )
      ),
      shiny::actionButton(ns("edit"), "Do"),
      shiny::tags$head(shiny::tags$style(
        shiny::HTML('#getToolbox-errors{color: red}')
      )),
      shiny::textOutput(ns("errors")),
      shiny::textOutput(ns("tooloutput")),
      shiny::br(),
      shiny::br(),
      shiny::tags$head(shiny::tags$style(
        shiny::HTML('#getToolbox-load{background-color:orange}')
      )),
      shiny::actionButton(ns("load"), "Upload to Database"),
      shiny::br(),
      shiny::br(),
      shiny::textOutput(ns("uploadoutput"))
    )
  }

###############################################################################
Toolbox_server <-
  function(input,
           output,
           session,
           connection,
           data,
           selected,
           active) {
    ###########################################################################
    shiny::observeEvent(input$edit, {
      shiny::req(data$ODMdata)
      shinyjs::disable("edit")
      tryCatch({
      shiny::withProgress(message = "Calculation in progress...", value = 1, {
        if (input$action == "apply_function") {
          ODMr:::get_values(data, selected, active()) %>%
            dplyr::mutate(DataValue = !!parse_quosure(input$apply),
                          QualifierID = 106) %>%
            dplyr::mutate(edited = TRUE) %>%
            ODMr:::upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(ODMr:::get_values(data, selected, active())), "values effected"
            )))
          })
        }
        if (input$action == "delete_values") {
          ODMr:::get_values(data, selected, active()) %>%
            dplyr::mutate(DataValue = NA, QualifierID = 102) %>%
            dplyr::mutate(edited = TRUE) %>%
            ODMr:::upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(ODMr:::get_values(data, selected, active())), "values set to NA"
            )))
          })
        }
        if (input$action == "flag_values") {
          ODMr:::get_values(data, selected,  active()) %>%
            dplyr::mutate(QualifierID = as.integer(input$flag)) %>%
            dplyr::mutate(edited = TRUE) %>%
            ODMr:::upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(ODMr:::get_values(data, selected, active())), "flags set to", input$flag
            )))
          })
        }
        if (input$action == "revert_raw") {
          GET <- ODMr:::get_values(data, selected,  active()) %>%
            dplyr::summarise(
              SiteID = min(SiteID),
              VariableID = min(VariableID),
              MethodID = min(MethodID),
              SourceID = min(SourceID),
              startDate = min(LocalDateTime),
              endDate = max(LocalDateTime)
            )
          odm_read(
            site_id = GET$SiteID,
            variable_id = GET$VariableID,
            method_id = GET$MethodID,
            level_id = 0,
            source_id = GET$SourceID,
            start_date = GET$startDate,
            end_date = GET$endDate,
            aggregate_by = "none",
            n = 2000000
          ) %>% dplyr::ungroup() %>%
            dplyr::select(LocalDateTime, DataValue, QualifierID) %>% dplyr::right_join(
              ODMr:::get_values(data, selected,  active()) %>% dplyr::select(-DataValue,-QualifierID),
              by = "LocalDateTime"
            ) %>%
            dplyr::mutate(edited = TRUE) %>%
            ODMr:::upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(ODMr:::get_values(data, selected, active())), "Values reverted to raw."
            )))
          })
        }
      })
      }, error = function(e) {
        output$errors <- shiny::renderText({
          paste(e)
        })
      })
      shinyjs::enable("edit")
    })
    ###########################################################################
    shiny::observeEvent(input$load, {
      shiny::req(data$ODMdata)
      shinyjs::disable("load")
      shiny::withProgress(message = "Loading to database...", value = 1, {
        if ("import" %in% names(data$ODMdata)) {
          upload <- data$ODMdata %>% dplyr::filter(import == TRUE) %>%
            dplyr::select(dplyr::one_of(
              c(
                "ValueID",
                "LocalDateTime",
                "DataValue",
                "UTCOffset",
                "SiteID",
                "VariableID",
                "QualifierID",
                "MethodID",
                "SourceID",
                "QualityControlLevelID"
              )
            ))
        } else if ("edited" %in% names(data$ODMdata)) {
          upload <- data$ODMdata %>% dplyr::filter(edited == TRUE) %>%
            dplyr::select(dplyr::one_of(
              c(
                "ValueID",
                "LocalDateTime",
                "DataValue",
                "UTCOffset",
                "SiteID",
                "VariableID",
                "QualifierID",
                "MethodID",
                "SourceID",
                "QualityControlLevelID"
              )
            ))
        }
        if (!("Aggregated" %in% names(data$ODMdata))) {
          label <-
            tidyr::unite(upload,
                         "label",
                         c("SiteID", "VariableID", "MethodID", "SourceID"),
                         sep = "_")
          upload <- split(upload, label$label)
          if ("import" %in% names(data$ODMdata)) {
            loadtext <- lapply(
              upload,
              odm_write,
              qc_check = 0,
              channel = connection,
              check_before_load = FALSE
            )
          } else {
            loadtext <- lapply(
              upload,
              odm_write,
              qc_check = 1,
              channel = connection,
              check_before_load = FALSE
            )
          }
          output$uploadoutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              sum(unlist(loadtext)), "data values effected"
            )))
          })
        }
      })
      shinyjs::enable("load")
    })
  }
