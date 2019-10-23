###############################################################################
Model_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::tableOutput(ns("NAstats")),
      shiny::selectInput(
        ns("action"),
        label = "Select Action",
        choices = list(
          `Pad Timeseries` =
            list("Complete timeseries" = "complete_data"),
          `Univariate imputation` = list("Linear" = "linear_infill_data",
                                         "Moving average" = "MA_infill_data"),
          `Multivariate imputation` = list(
            "Predictive mean matching" = "pmm",
            "Weighted PMM" = "midastouch",
            "CART" = "cart",
            "Random forest" = "rf"
          )
        ),
        selected = NULL
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'complete_data'"),
        shiny::textInput(ns("complete_by"), label = "Fill dates by", value = "day")
      ),
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("action"),
          "'] == 'linear_infill_data' |
                         input['",
          ns("action"),
          "'] == 'MA_infill_data'"
        ),
        shiny::numericInput(ns("max_gap"), label = "Max size gap to fill:", value = 3)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] == 'MA_infill_data'"),
        shiny::numericInput(ns("window"), label = "Moving window width:", value = 4)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("action"), "'] != 'complete_data'"),
        shiny::radioButtons(
          ns("multiple_action"),
          label = "Action:",
          c("Validation" = "test",
            "Infill NAs" = "infill"),
          selected = NULL
        )
      ),
      shiny::actionButton(ns("edit"), "Do"),
      shiny::tags$head(shiny::tags$style(
        shiny::HTML('#getModel-errors{color: red}')
      )),
      shiny::textOutput(ns("errors")),
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("multiple_action"),
          "'] == 'test' &
                         input['",
          ns("action"),
          "'] != 'complete_data'"
        ),
        shiny::tableOutput(ns("kfolds"))
      ),
      shiny::br(),
      shiny::br(),
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("multiple_action"),
          "'] != 'test' |
                         input['",
          ns("action"),
          "'] == 'complete_data'"
        )
      ),
      shiny::textOutput(ns("tooloutput"))
    )
  }

###############################################################################
Model_server <-
  function(input,
           output,
           session,
           connection,
           data,
           selected,
           active) {
    ###########################################################################
    output$NAstats <- shiny::renderTable({
      shiny::req(data$ODMdata)
      res <- rle(is.na(
        getValues(data, selected, active()) %>%
        dplyr::arrange(LocalDateTime) %>% .$DataValue))
      out <- res$lengths * res$values
      data.frame(
        n = sum(out),
        "Longest Gap" = max(out),
        "Most Frequent Gap" = which.max(tabulate(out[out>0])),
        "Shortest Gap" = min(out[out>0]))
    })
    ###########################################################################
    shiny::observeEvent(input$edit, {
      shiny::req(data$ODMdata)
      output$errors <- NULL
      shinyjs::disable("edit")
      tryCatch({
        shiny::withProgress(message = "Calculation in progress...", value = 1, {
          if (input$action == "pmm" |
              input$action == "midastouch" |
              input$action == "cart" |
              input$action == "rf") {
            # MICE Imputation
            data_L <-
              data$ODMdata %>% dplyr::select(LocalDateTime, DataValue, label) %>%
              dplyr::distinct() %>% tidyr::spread(label, DataValue)
            if (input$multiple_action == "test") {
              k = 10
              val <- na.omit(data_L)
              train <- na.omit(data_L)
              # folds for k-folds cross-validation
              folds = sample(rep(1:k, length = nrow(val)))
              MAE = data_L[0, -1]
              RMSE = data_L[0, -1]
              # do k-folds cross validation
              for (i in 1:k) {
                for (p in 2:ncol(train)) {
                  train[folds == sample(folds, 1), p] <- NA
                }
                mat <- mice::make.predictorMatrix(train)
                mat[,1] = 0
                mat[1,] = 0
                out = mice::mice(
                  train,
                  method = input$action,
                  predictorMatrix = mat,
                  m = 3,
                  maxit = 10,
                  seed = 0123
                )
                com <- mice::complete(out, action = "long") %>%
                  dplyr::group_by(LocalDateTime) %>%
                  dplyr::select(-.imp,-.id) %>%
                  dplyr::summarise_all(median)
                test <- val[, -1] - com[, -1]
                test[test == 0] <- NA
                MAE[i, ] <-
                  apply(abs(test), 2, function(x)
                    mean(x, na.rm = TRUE))
                RMSE[i, ] <-
                  apply(test, 2, function (x)
                    sqrt(mean((x) ^ 2, na.rm = TRUE)))
              }
              MAE <- dplyr::summarise_all(MAE, mean)
              RMSE <- dplyr::summarise_all(RMSE, mean)
              output$kfolds <- shiny::renderTable({
                dplyr::bind_rows("MAE" = MAE,
                                 "RMSE" = RMSE,
                                 .id = "id")
              },
              digits = 3)
            }

            if (input$multiple_action == "infill") {
              mat <- mice::make.predictorMatrix(data_L)
              mat[,1] = 0
              mat[1,] = 0
              mice_out <-
                mice::mice(
                  data_L,
                  method = input$action,
                  predictorMatrix = mat,
                  m = 10,
                  maxit = 10,
                  seed = 0123
                )
              imputed <-
                mice::complete(mice_out, action = "long") %>%
                dplyr::group_by(LocalDateTime) %>%
                dplyr::select(-.imp,-.id) %>%
                dplyr::summarise_all(median) %>%
                tidyr::gather(label, DataValue,-LocalDateTime)
              where <-
                as.data.frame(mice_out$where) %>% dplyr::mutate(LocalDateTime = data_L$LocalDateTime) %>%
                tidyr::gather(label, where,-LocalDateTime)
              imputed <- dplyr::left_join(imputed, where)
              data$ODMdata <-
                dplyr::left_join(data$ODMdata,
                                 imputed,
                                 by = c("LocalDateTime", "label")) %>%
                dplyr::mutate(
                  DataValue = DataValue.y,
                  QualifierID = ifelse(where == TRUE, 105, QualifierID),
                  QualityControlLevel = 2
                ) %>%
                dplyr::select(-DataValue.x,-DataValue.y,-where)
              output$tooloutput <- shiny::renderText({
                shiny::isolate(paste("DataValues infilled by ", input$action))
              })
            }
          }

          ### imputeTS imputation ###
          if (input$action == "linear_infill_data" |
              input$action == "MA_infill_data") {
            if (input$multiple_action == "test") {
              k = 10
              val <- getValues(data, selected, active()) %>%
                dplyr::arrange(LocalDateTime) %>%
                dplyr::select(LocalDateTime, DataValue) %>%
                na.omit()
              train <- val
              folds = rep(rep(sample(1:k), each = input$max_gap), length = nrow(train))
              MAE = val[0, -1]
              RMSE = val[0, -1]
              for (i in 1:k) {
                for (p in 2:ncol(train)) {
                  train[folds == sample(folds, 1), p] <- NA
                }
                if (input$action == "linear_infill_data") {
                  train$DataValue <-
                    imputeTS::na.interpolation(train$DataValue, maxgap = input$max_gap)
                }
                if (input$action == "MA_infill_data") {
                  train$DataValue <-
                    imputeTS::na.ma(train$DataValue,
                                    k = input$window,
                                    maxgap = input$max_gap,
                                    weighting = "linear")
                }
                test <- val[, -1] - train[, -1]
                test[test == 0] <- NA
                MAE[i, ] <-
                  apply(abs(test), 2, function(x)
                    mean(x, na.rm = TRUE))
                RMSE[i, ] <-
                  apply(test, 2, function (x)
                    sqrt(mean((x) ^ 2, na.rm = TRUE)))
              }
              MAE <- dplyr::summarise_all(MAE, mean)
              RMSE <- dplyr::summarise_all(RMSE, mean)
              output$kfolds <- shiny::renderTable({
                dplyr::bind_rows("MAE" = MAE,
                                 "RMSE" = RMSE,
                                 .id = "id")
              },
              digits = 3)
            }

            if (input$multiple_action == "infill") {
              vals <- getValues(data, selected, active()) %>%
                dplyr::arrange(LocalDateTime)
              res = rle(is.na(vals$DataValue))
              vals$L <- rep(res$values*res$lengths,res$lengths)
              vals <- dplyr::filter(vals, L <= input$max_gap)
              if (input$action == "linear_infill_data") {
                vals$DataValue <-
                  imputeTS::na.interpolation(vals$DataValue, maxgap = input$max_gap)
              }
              if (input$action == "MA_infill_data") {
                vals$DataValue <-
                  imputeTS::na.ma(vals$DataValue,
                                  k = input$window,
                                  maxgap = input$max_gap,
                                  weighting = "linear")
              }
              vals %>%
                dplyr::mutate(edited = ifelse(QualifierID == 101 & !is.na(DataValue) |
                                                QualifierID == 102 & !is.na(DataValue),
                                              TRUE, edited)) %>%
                dplyr::mutate(QualifierID = ifelse(
                  QualifierID == 101 & !is.na(DataValue) |
                    QualifierID == 102 & !is.na(DataValue),
                  105,
                  QualifierID
                )) %>%
                dplyr::select(-L) %>%
                upsert(data, .)
              output$tooloutput <- shiny::renderText({
                shiny::isolate(cat(paste("DataValues infilled by ", input$action)))
              })
            }
          }

          ### Pad timeseries ###
          if (input$action == "complete_data") {
            getValues(data, selected, active()) %>%
              dplyr::arrange(LocalDateTime) %>%
              tidyr::complete(LocalDateTime = seq(
                min(LocalDateTime),
                max(LocalDateTime),
                by = input$complete_by
              )) %>%
              dplyr::mutate(QualifierID = ifelse(is.na(QualityControlLevelID), 999, QualifierID)) %>%
              tidyr::fill(UTCOffset,
                          SiteID,
                          VariableID,
                          MethodID,
                          SourceID,
                          QualityControlLevelID) %>%
              dplyr::filter(QualifierID == 999) %>%
              dplyr::mutate(index = max(data$ODMdata[, "index"]) + seq_len(length(DataValue))) %>%
              dplyr::mutate(ValueID = NA) %>%
              dplyr::mutate(edited = TRUE) %>%
              dplyr::mutate(QualifierID = 101) %>%
              tidyr::unite(
                label,
                SiteID,
                VariableID,
                MethodID,
                QualityControlLevelID,
                SourceID,
                remove = FALSE
              ) %>%
              dplyr::mutate(label = paste0("TS", label)) %>%
              upsert(data, .)
            output$tooloutput <- shiny::renderText({
              shiny::isolate(cat(paste("Dates infilled by ", input$complete_by)))
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
  }
