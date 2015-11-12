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
#'@param aes x,y,... List of name value pairs giving aesthetics to map
#'@param geom or list of geoms to draw. Defaults to geom_point()
#'
#'@examples
#'\dontrun{
#'df <- data.frame(x = rnorm(100), y = rnorm(100))
#'pint(df, aes(x, y))
#'pint(df, aes(x, y), geom = geom_line())
#'}
#'
#'@import ggplot2
#'@export

pint <- function(data, aes, geom = geom_point()) {

  shiny::shinyApp(ui <- shiny::basicPage(
    shiny::plotOutput("plot", brush = "plot_brush", dblclick = "plot_dblclick"),
    shinyAce::aceEditor("code",
      "#brushed() \n#summary(brushed()) \n#values <<- editData(values, index(brushed()), FUN = function (x) x + 1)",
      mode = "r", height = "100px"),
    shiny::actionButton("eval", "Enter"),
    shiny::verbatimTextOutput("output")),

    server <- function(input, output, session) {

      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      values <- data

      brushed <- function(df = values, brush = input$plot_brush, ...) {
        return(brushedPoints(df = df, brush = brush, ...))
      }

      output$output <- shiny::renderPrint({
        input$eval
        return(shiny::isolate(eval(parse(text = input$code))))
      })

      output$plot <- shiny::renderPlot({
        input$eval
        ggplot(
          if (xts::is.xts(values)) {
            zoo::fortify.zoo(values)
          }
          else {
            values
          },
          aes) + geom +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) + theme_minimal()
      })

      shiny::observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          if (TRUE %in%  sapply(values , is.POSIXct) | xts::is.xts(values)) {
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
