library(dplyr)
library(shinyAce)

ODMtools <- function(ODMdata, Refdata = NULL, ...) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("ODMtools"),
    miniUI::miniContentPanel(
      shiny::fillRow(flex = c(2,1),
        shiny::fillCol(
          rbokeh::rbokehOutput("rbokeh", width = "100%", height = "100%")),
        shiny::fillCol(flex = NA,
          aceEditor("code", mode="r", height = 100),
          actionButton("eval", "Evaluate"),
          br(),
          verbatimTextOutput("output"))
      )
    )
  )

  server <- function(input, output, session) {
    values <- reactiveValues(ODMdata = ODMdata)

    Data <- function (x = NULL, rows = input$selection_info + 1) {
      if(length(rows) == 0) rows <- 1:nrow(values$ODMdata)
      if(!is.null(x)) values$ODMdata[rows,] <- x
      values$ODMdata[rows,]
    }

    output$rbokeh <- rbokeh::renderRbokeh({
      P = rbokeh::figure(webgl = TRUE, lod_threshold = 0,
        width = 1024, height = 600,
        tools = c("pan", "box_zoom", "resize", "reset")) %>%
        rbokeh::ly_points(x = LocalDateTime, y = DataValue, data = values$ODMdata,
          glyph = 1, size = 5, lname = "points", ...) %>%
                rbokeh::x_axis(label = "LocalDateTime") %>%
        rbokeh::y_axis(label = "DataValue") %>%
        rbokeh::tool_tap(rbokeh::shiny_callback("selection_info"), "points") %>%
        rbokeh::tool_box_select(rbokeh::shiny_callback("selection_info"), "points") %>%
        rbokeh::tool_lasso_select(rbokeh::shiny_callback("selection_info"), "points")
      if(!is.null(Refdata)) ({
        P = P %>% rbokeh::ly_points(x = LocalDateTime, y = DataValue, data = Refdata,
          glyph = 1, size = 5, lname = "reference", line_color = "grey", ...)
      })
      P
    })
    observe({
      updateAceEditor(session, "code",
        isolate(eval(parse(text=input$code))),
        mode="r")
    })
    output$output <- renderPrint({
      input$eval
      return(isolate(eval(parse(text=input$code))))
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = values$ODMdata)
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
