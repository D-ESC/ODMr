Console_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyAce::aceEditor(ns("code"), mode = "r", height = 200),
    shiny::actionButton(ns("eval"), "Evaluate"),
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput(ns("output")),
    shiny::helpText(
      "Access the full dataframe loaded into ODMtools",
      shiny::br(),
      "> data$ODMdata",
      shiny::br(), shiny::br(),
      "Retrieve just selected values",
      shiny::br(),
      "> getValues(data, selected, active())",
      shiny::br(), shiny::br(),
      "Write any modifications back to the dataframe",
      shiny::br(),
      "> upsert(data, .)",
      shiny::br(), shiny::br(),
      "Must set/create column 'edited' and set fo TRUE for any edited rows",
      shiny::br(),
      "Example:",
      shiny::br(),
      "> getValues(data, selected, active()) %>%",
      shiny::br(),
      "dplyr::mutate(DataValue = DataValue + 10,",
      shiny::br(),
      "QualifierID = 106,",
      shiny::br(),
      "edited = TRUE) %>%",
      shiny::br(),
      "upsert(data, .)"
    )
  )
}

Console_server <- function(input, output, session, data, selected, active) {
  shiny::observe({
    shinyAce::updateAceEditor(session, "code",
                              shiny::isolate(eval(parse(text = input$code))),
                              mode = "r"
    )
  })
  output$output <- shiny::renderPrint({
    input$eval
    return(shiny::isolate(eval(
      parse(text = stringr::str_replace_all(input$code, "[\r]", ""))
    )))
  })
}
