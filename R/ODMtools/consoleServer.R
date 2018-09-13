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
