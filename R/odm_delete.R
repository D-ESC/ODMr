#'Delete records from ODM database
#'
#'The function odm_delete can be used to delete data from ODM database.
#'
#'A standard SQL query is issued to the ODM database and the values are
#'removed. The data deleted can be limited by a start
#'and/or end date defined by the user.
#'
#'@param site_id index value for the location at which the observation was made
#'@param variable_id index value for the variable that the data represents
#'@param method_id index value for the method used to collect the observation
#'@param source_id index value for the source of the observation
#'@param level_id the level of quality control processing
#'@param start_date access data from this date forward
#'@param end_date access data up to this date
#'@param channel connection handle as returned by odbcConnect
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
#'  UID = "update", PWD = rstudioapi::askForPassword("Database password"),
#'  Port = 1433)
#'
#'# Delete data by site, variable, method and QC level.
#'odm_delete(site_id = 1, variable_id = 1, method_id = 9,
#'  level_id = 0, start_date = "2013-06-01", end_date = "2013-07-01",
#'  channel = ODM)
#'  }
#'
#'@export

odm_delete <- function(site_id,
                       variable_id,
                       method_id,
                       level_id,
                       source_id = 1,
                       start_date = "1970-01-1 00:00:00",
                       end_date = Sys.Date(),
                       channel = ODM) {
  catalog <- odm_read_tbl(odm_tbl = "Seriescatalog", channel = channel) %>%
    dplyr::filter(
      SiteID == site_id,
      VariableID == variable_id,
      MethodID == method_id,
      QualityControlLevelID == level_id,
      SourceID == source_id
    )
  cat(
    "deleting: ",
    catalog$SiteCode,
    catalog$VariableCode,
    catalog$MethodDescription,
    "\n"
  )
  question1 <- readline("Would you like to proceed? (Y/N) ")
  stopifnot(regexpr(question1, "y", ignore.case = TRUE) == 1)

  Data <- DBI::dbExecute(channel, {
    paste(
      "DELETE
           FROM DataValues
           WHERE     (DataValues.SiteID IN (",
      paste(site_id, collapse = ","),
      "))
           AND (DataValues.VariableID IN (",
      paste(variable_id, collapse = ","),
      "))
           AND (DataValues.MethodID IN (",
      paste(method_id, collapse = ","),
      "))
           AND (DataValues.QualityControlLevelID IN (",
      paste(level_id, collapse = ","),
      "))
           AND ((DataValues.LocalDateTime > '",
      start_date,
      "')
           AND (DataValues.LocalDateTime < '",
      end_date,
      "'))",
      sep = ""
    )
  })
  return(Data)
}
