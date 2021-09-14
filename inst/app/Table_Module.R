###############################################################################
table_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(shinycssloaders::withSpinner(
      DT::dataTableOutput(ns("Dtbl2"))))
  }

###############################################################################
table_server <-
  function(id,
           data,
           selected,
           active) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
    ###########################################################################
    output$Dtbl2 <- DT::renderDataTable({
      shiny::req(data$ODMdata)
      data$ODMdata %>%
        dplyr::semi_join(data$meta[active(), ]) %>%
        dplyr::filter(index %in% selected()) %>%
        dplyr::mutate(LocalDateTime = as.character(LocalDateTime)) %>%
        dplyr::select(LocalDateTime,
                      DataValue,
                      UTCOffset,
                      SiteID,
                      VariableID,
                      QualifierID,
                      MethodID,
                      SourceID,
                      QualityControlLevelID) %>%
        DT::datatable(filter = "top", style = "bootstrap")
    })
      } 
  )}
