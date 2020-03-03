#' ODM read function
#'
#' This function can retrieve meta data from an ODM database by table
#' name and ID
#'
#' @param odm_tbl name of the table in the ODM database
#' @param ID id of entry in the table you are referencing
#' @param channel connection handle as returned by odbcConnect
#' (default value = ODM)
#'
#' @return A data frame
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
#' odm_read_tbl(odm_tbl = "Sites")
#'
#' # Get a list of data series in the database
#' odm_read_tbl(odm_tbl = "Seriescatalog")
#' }
#'
#' @export

odm_read_tbl <- function(odm_tbl = "Seriescatalog", ID = NULL, channel = ODM) {
  index <- channel %>%
    dplyr::tbl(odm_tbl) %>%
    {.$ops$vars[1]}
  tmp <- channel %>% dplyr::tbl(odm_tbl) %>%
    {if (!is.null(ID)) dplyr::filter(., !!dplyr::sym(index) %in% ID)
      else .} %>%
    dplyr::collect()
  return(tmp)
}
