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
      choices = list(
        "linear" = "approx",
        "spline" = "spline",
        "last observation" = "locf"
      ),
      selected = "linear",
      inline = TRUE
    ),
    shiny::numericInput("max_gap", label = "Max gap", value = Inf)
  ),
  shiny::actionButton("edit", "Do")
)
