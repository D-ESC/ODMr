#' ODM write function
#'
#' This function can write meta data to an ODM database table
#'
#' @param x dtaframe to load to ODM table
#' @param odm_tbl name of the table in the ODM database
#' @param channel connection handle as returned by odbcConnect
#' (default value = ODM)
#'
#' @return number of rows effected
#'
#' @examples
#'
#' \dontrun{
#' # Establish connection with database
#' ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
#'  UID = "update", PWD = rstudioapi::askForPassword("Database password"),
#'  Port = 1433)
#'
#' # Get a list of sites in the database
#' Site <- odm_read_tbl(odm_tbl = "Sites")
#'
#' # Get a list of data series in the database
#' odm_write_tbl(Site, odm_tbl = "Sites")
#' }
#'
#' @export

odm_write_tbl <- function(x, odm_tbl = "Seriescatalog", channel = ODM) {
  if(odm_tbl == "DataValues")
    stop("Please use odm_write to add or change values in the DataValues table")

  primary_key <- DBI::dbGetQuery(ODM, glue::glue("SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA + '.' + QUOTENAME(CONSTRAINT_NAME)), 'IsPrimaryKey') = 1
AND TABLE_NAME = '{odm_tbl}'"))

  col_names <- ODM %>% dplyr::tbl(odm_tbl) %>% {.$ops$vars}
  if (any(!(names(x) %in% col_names)))
    stop(glue::glue("Table names must contain {col_names}"))

  tmp <- sqlmerge(x, TableName = odm_tbl,
           By = primary_key,
           Key = primary_key,
           channel = channel)
  return(tmp)
}
