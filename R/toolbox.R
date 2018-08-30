UI_Tools <- function() {
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
  )
}

Server_Tools <- function() {
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
}
