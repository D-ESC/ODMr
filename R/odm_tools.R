#'Launch shiny app to work interactively with ODM data
#'
#'The function odm_tools can be used to launch a web based tool to plot and
#'correct data in an ODM database.
#'
#'@examples
#'
#'\dontrun{
#'odm_tools()
#'}
#'
#'@export

odm_tools <- function() {
  appdir <- system.file("app", package = "ODMr")
  shiny::runApp(appdir, launch.browser = TRUE)
}
