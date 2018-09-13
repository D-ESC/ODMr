output$DTable <- DT::renderDataTable({
  shiny::req(plotly::event_data("plotly_selected", source = "subset"))
  dt <-
    DT::datatable(Data(), style = "bootstrap", editable = TRUE)
})

shiny::observeEvent(input$DTable_cell_edit, {
  info <- input$DTable_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  d <- Data()
  d[i, j] <- DT::coerceValue(v, Data()[[i, j]])
  d %>% Data()
  # replaceData(proxy2, d2, resetPaging = FALSE)  # important
})
