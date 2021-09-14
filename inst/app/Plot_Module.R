Plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot"), height = "600px")),
    shiny::selectInput(inputId = ns("plot_color"),
                       label = "Colour/Group By:", choices = c("Series" = "label",
                                                               "Qualifier" = "QualifierID"))
  )
}

Plot_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$plot <- plotly::renderPlotly({
        shiny::req(data$ODMdata)
        data$ODMdata %>%
          plotly::plot_ly(
            x = ~LocalDateTime,
            y = ~DataValue,
            key = data$ODMdata$index,
            split = ~get(input$plot_color),
            type = "scattergl",
            mode = "markers",
            opacity = 0.8) %>%
          plotly::layout(legend = list(orientation = "h"))
      })
  
  selected <- reactive({
    key = plotly::event_data("plotly_selected")$key
    if(length(key) < 1) {
      key = 1:nrow(data$ODMdata)
    }
    key
  })
  
  return(selected)
    })
}
