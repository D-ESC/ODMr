shinydashboard::box(
  width = NULL,
  collapsible = TRUE,
  shinyAce::aceEditor("code", mode = "r", height = 200),
  shiny::actionButton("eval", "Evaluate"),
  shiny::br(),
  shiny::br(),
  shiny::verbatimTextOutput("output")
)
