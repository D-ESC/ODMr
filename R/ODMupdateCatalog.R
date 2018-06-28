#' Update ODM series catalog
#'
#' The function can be used to trigger the stored procedure in a MS-SQL
#' database for updating the series catalog.
#'
#' @param channel connection handle as returned by odbcConnect (default value = ODM)
#'
#' @examples
#'
#' \dontrun{
#' # Establish connection with database
#' ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
#'  UID = "sa", PWD = rstudioapi::askForPassword("Database password"),
#'  Port = 1433)
#'
#' # Update catalog
#' ODMupdateCatalog()
#' }
#'
#' @export

ODMupdateCatalog <- function(channel = ODM) {
  DBI::dbExecute(channel, "OD.dbo.spUpdateSeriesCatalog")
}
