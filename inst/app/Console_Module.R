console_ui <- function(id) {
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
      "> DataValues()",
      shiny::br(), shiny::br(),
      "Retrieve just selected values",
      shiny::br(),
      "> Selected()",
      shiny::br(), shiny::br(),
      "Write any modifications back to the dataframe",
      shiny::br(),
      "> Upsert()",
      shiny::br(), shiny::br(),
      "Example:",
      shiny::br(),
      "> Selected() %>%",
      shiny::br(),
      "dplyr::mutate(DataValue = DataValue + 10,",
      shiny::br(),
      "QualifierID = 106) %>%",
      shiny::br(),
      "Upsert()"
    )
  )
}

console_server <- function(id, data, selected, active) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      DataValues <- reactive({data$ODMdata})
      Selected <- reactive({ODMr:::get_values(data, selected, active())})
      Upsert <- function(df) {ODMr:::upsert(data = data, insert = df)}
      
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
    })
}
