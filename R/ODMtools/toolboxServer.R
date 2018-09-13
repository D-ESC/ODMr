shiny::observeEvent(input$edit, {
  if (input$action == "shift_values") {
    Data() %>% dplyr::mutate(DataValue = DataValue + input$shift,
                             QualifierID = 106) %>% Data()
  }
  if (input$action == "set_values") {
    Data() %>% dplyr::mutate(DataValue = input$set, QualifierID = 105) %>% Data()
  }
  if (input$action == "drift_values") {
    len <- length(Data()$DataValue)
    drift <- (input$drift2 - input$drift1) / len * 1:len
    Data() %>% dplyr::mutate(DataValue = DataValue + drift,
                             QualifierID = 106) %>% Data()
  }
  if (input$action == "delete_values") {
    Data() %>% dplyr::mutate(DataValue = NA, QualifierID = 102) %>% Data()
  }
  if (input$action == "flag_values") {
    Data() %>% dplyr::mutate(QualifierID = as.integer(input$flag)) %>% Data()
  }
})
