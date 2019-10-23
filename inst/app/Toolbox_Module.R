###############################################################################
Toolbox_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::selectInput(
        ns("action"),
        label = "Select Action",
        choices = list(
          "Shift Values" = "shift_values",
          "Shift Dates" = "shift_dates",
          "Set Values" = "set_values",
          "Drift Correction" = "drift_values",
          "Apply Function" = "apply_function",
          "Test for Outliers" = "test_outliers",
          "Revert To Raw Values" = "revert_raw",
          "Delete Values" = "delete_values",
          "Set Flag" = "flag_values"
        ),
        selected = NULL
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'shift_values'"),
        shiny::numericInput(ns("shift"), label = "Shift values", value = NULL)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'shift_dates'"),
        shiny::numericInput(ns("shift_date"), label = "Shift dates", value = NULL)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'set_values'"),
        shiny::numericInput(ns("set"), label = "Set values", value = NULL)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'drift_values'"),
        shiny::splitLayout(
          shiny::numericInput(ns("drift1"), label = "Start value", value = 1),
          shiny::numericInput(ns("drift2"), label = "End value", value = 1)
        )
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'apply_function'"),
        shiny::textInput(ns("apply"), label = "Apply function", value = NULL)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'test_outliers'"),
        shiny::splitLayout(
          shiny::numericInput(ns("outlier_alpha"), label = "alpha", value = 0.025),
          shiny::numericInput(ns("outlier_max_anom"), label = "max anomalies", value = 0.05)
        ),
        shiny::splitLayout(
          shiny::numericInput(ns("outlier_freq"), label = "frequency", value = 0),
          shiny::numericInput(ns("outlier_trend"), label = "trend", value = 0)
        ),
        shiny::radioButtons(
          ns("outlier_action"),
          label = "Action:",
          c("Test for outliers" = "test",
            "Remove outliers" = "remove")
        )
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
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("outlier_action"),
          "'] == 'test' &
                         input['",
          ns("action"),
          "'] == 'test_outliers'"
        ),
        shiny::plotOutput(ns("plot_outliers"))
      ),
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
        if (input$action == "shift_values") {
          getValues(data, selected, active()) %>%
            dplyr::mutate(DataValue = DataValue + input$shift,
                          QualifierID = 106) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "values shifted by", input$shift
            )))
          })
        }
        if (input$action == "shift_dates") {
          edited <- getValues(data, selected, active()) %>%
            dplyr::mutate(LocalDateTime = LocalDateTime + input$shift_date * 60)
          edited <- getValues(data, selected, active()) %>%
            dplyr::full_join(edited, by = "LocalDateTime", suffix = c("", ".y")) %>%
            dplyr::mutate(DataValue = DataValue.y, edited = TRUE,
                          QualifierID = ifelse(is.na(DataValue), 101, 106)) %>%
            dplyr::select(-dplyr::ends_with(".y")) %>%
            tidyr::fill(-ValueID, -DataValue, .direction = "downup") %>%
            dplyr::semi_join(data$ODMdata, by = "LocalDateTime")
          data$ODMdata <- data$ODMdata %>%
            dplyr::anti_join(edited,
                             by = c("LocalDateTime", "SiteID", "VariableID", "MethodID", "SourceID", "QualityControlLevelID")) %>%
            dplyr::bind_rows(edited)

          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "dates shifted by", input$shift_date
            )))
          })
        }
        if (input$action == "set_values") {
          getValues(data, selected, active()) %>%
            dplyr::mutate(DataValue = input$set, QualifierID = 105) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "values set to", input$set
            )))
          })
        }
        if (input$action == "drift_values") {
          len <- nrow(getValues(data, selected, active()))
          drift <- (input$drift2 - input$drift1) / len * 1:len
          getValues(data, selected, active()) %>%
            dplyr::mutate(DataValue = DataValue + drift,
                          QualifierID = 106) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "values adjusted"
            )))
          })
        }
        if (input$action == "apply_function") {
          getValues(data, selected, active()) %>%
            dplyr::mutate(DataValue = !!parse_quosure(input$apply),
                          QualifierID = 106) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "values effected"
            )))
          })
        }
        if (input$action == "test_outliers") {
          outliers <- getValues(data, selected, active()) %>%
            dplyr::filter(!is.na(DataValue)) %>%
            anomalize::time_decompose(DataValue, merge = TRUE,
                                      frequency = if (input$outlier_freq == 0) {"auto"} else {input$outlier_freq},
                                      trend = if (input$outlier_trend == 0) {"auto"} else {input$outlier_trend}) %>%
            anomalize::anomalize(
              remainder, method = "gesd",
              alpha = input$outlier_alpha,
              max_anoms = input$outlier_max_anom
            )
          if (input$outlier_action == "test") {
            output$plot_outliers <-
              shiny::renderPlot(
                anomalize::time_recompose(outliers) %>% anomalize::plot_anomalies(time_recomposed = TRUE)
              )
          }
          if (input$outlier_action == "remove") {
            outliers %>%
              dplyr::filter(anomaly == "Yes") %>%
              dplyr::mutate(DataValue = NA, QualifierID = 102) %>%
              dplyr::mutate(edited = TRUE) %>%
              dplyr::select(
                -observed,-season,-trend,-remainder,-remainder_l1,-remainder_l2,-anomaly
              ) %>%
              upsert(data, .)
            output$tooloutput <- shiny::renderPrint({
              shiny::isolate(cat(paste(
                nrow(outliers %>% dplyr::filter(anomaly == "Yes")),
                "values set to NA"
              )))
            })
          }
        }
        if (input$action == "delete_values") {
          getValues(data, selected, active()) %>%
            dplyr::mutate(DataValue = NA, QualifierID = 102) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "values set to NA"
            )))
          })
        }
        if (input$action == "flag_values") {
          getValues(data, selected,  active()) %>%
            dplyr::mutate(QualifierID = as.integer(input$flag)) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "flags set to", input$flag
            )))
          })
        }
        if (input$action == "revert_raw") {
          GET <- getValues(data, selected,  active()) %>%
            dplyr::summarise(
              SiteID = min(SiteID),
              VariableID = min(VariableID),
              MethodID = min(MethodID),
              SourceID = min(SourceID),
              startDate = min(LocalDateTime),
              endDate = max(LocalDateTime)
            )
          ODMr::ODMgetData(
            SiteID_ = GET$SiteID,
            VariableID_ = GET$VariableID,
            MethodID_ = GET$MethodID,
            QualityControlLevelID_ = 0,
            SourceID_ = GET$SourceID,
            startDate = GET$startDate,
            endDate = GET$endDate,
            AggregateBy = "none"
          ) %>% dplyr::ungroup() %>%
            dplyr::select(LocalDateTime, DataValue, QualifierID) %>% dplyr::right_join(
              getValues(data, selected,  active()) %>% dplyr::select(-DataValue,-QualifierID),
              by = "LocalDateTime"
            ) %>%
            dplyr::mutate(edited = TRUE) %>%
            upsert(data, .)
          output$tooloutput <- shiny::renderPrint({
            shiny::isolate(cat(paste(
              nrow(getValues(data, selected, active())), "Values reverted to raw."
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
              ODMr::ODMload,
              QCcheck = 0,
              channel = connection,
              check_before_load = FALSE
            )
          } else {
            loadtext <- lapply(
              upload,
              ODMr::ODMload,
              QCcheck = 1,
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
