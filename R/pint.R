#'Interactive plot (pint)
#'
#'Visually explore data and highlight and interact with observations through
#'data brushing. A simple console allows for any R code to be run against
#'highlighted values including adding, removing, changing and summarizing.
#'
#'Uses ggplot2 for producing the plots within a shiny app. The first two
#'arguments are "x" and "y", giving the x- and y-coordinates for the objects on
#'the plot. There is also a data argument used to declare the input data frame.
#'
#'@param x,y Name value pairs giving aesthetics to map
#'@param data Data frame to use
#'@param geom or list of geoms to draw. Defaults to geom_point()
#'
#'@examples
#'df <- data.frame(x = rnorm(100), y = rnorm(100))
#'pint("x", "y", df)
#'pint("x","y",df, geom = geom_line())
#'
#'@import ggplot2
#'@export

pint <- function(x, y, data, geom = geom_point()) {

  is.POSIXct <- function(x) inherits(x, "POSIXct")

  shiny::shinyApp(ui <- shiny::basicPage(
    shiny::plotOutput("plot", brush = "plot_brush", dblclick = "plot_dblclick"),
    shinyAce::aceEditor("code",
      "#values <<- brushed() %>% mutate(x = ifelse(selected_ == TRUE, x, x))",
      mode = "r", height = "100px"),
    shiny::actionButton("eval", "Enter"),
    shiny::verbatimTextOutput("output")),

    server <- function(input, output, session) {

      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      values <- data

      brushed <- function(df = values, brush = input$plot_brush, ...) {
        return(shiny::brushedPoints(df = df, brush = brush, ...))
      }

      output$output <- shiny::renderPrint({
        input$eval
        return(shiny::isolate(eval(parse(text = input$code))))
      })

      output$plot <- shiny::renderPlot({
        input$eval
        ggplot(values, aes_string(x, y)) + geom +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) + theme_minimal()
      })

      shiny::observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          if (is.POSIXct(values[, x])) {
            ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
          }
          else {
            ranges$x <- c(brush$xmin, brush$xmax)
          }
          ranges$y <- c(brush$ymin, brush$ymax)
        }
        else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
    }
  )
}
