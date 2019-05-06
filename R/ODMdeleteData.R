#'Delete records from ODM database
#'
#'The function ODMdeleteData can be used to delete data from ODM database.
#'
#'A standard SQL query is issued to the ODM database and the values are
#'removed. The data deleted can be limited by a start
#'and/or end date defined by the user.
#'
#'Data series from the catalog may be specified using the series catalog
#'ID. Since the series catalog is not stable through time you may also delete
#'values by specifying SiteID, VariableID and MethodID.
#'
#'@param SeriesID index value for a specific record within the series catalog
#'@param SiteID index value for the location at which the observation was made
#'@param VariableID index value for the variable that the data represents
#'@param MethodID index value for the method used to collect the observation
#'@param QCLevelID the level of quality control processing
#'@param startDate access data from this date forward
#'@param endDate access data up to this date
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
#'ODMdeleteData(SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01",
#'  channel = ODM)
#'  }
#'
#'@export

ODMdeleteData <- function(SiteID_,
                          VariableID_,
                          MethodID_,
                          QualityControlLevelID_,
                          SourceID_ = 1,
                          startDate = "1970-01-1 00:00:00",
                          endDate = Sys.Date(), channel = ODM) {

  Catalog <- ODMgetCatalog(channel) %>%
    dplyr::filter(SiteID == SiteID_,
                  VariableID == VariableID_,
                  MethodID == MethodID_,
                  QualityControlLevelID == QualityControlLevelID_,
                  SourceID == SourceID_)
  cat("deleting: ", Catalog$SiteCode, Catalog$VariableCode, Catalog$MethodDescription, "\n")
  question1 <- readline("Would you like to proceed? (Y/N) ")
  stopifnot(regexpr(question1, 'y', ignore.case = TRUE) == 1)

  Data <- DBI::dbExecute(channel, {
    paste ("DELETE
           FROM DataValues
           WHERE     (DataValues.SiteID IN (", paste(SiteID_, collapse = ","), "))
           AND (DataValues.VariableID IN (", paste(VariableID_, collapse = ","), "))
           AND (DataValues.MethodID IN (", paste(MethodID_, collapse = ","), "))
           AND (DataValues.QualityControlLevelID IN (", paste(QualityControlLevelID_, collapse = ","), "))
           AND ((DataValues.LocalDateTime > '", startDate, "')
           AND (DataValues.LocalDateTime < '", endDate, "'))", sep = "")
  })
  DBI::dbExecute(channel, {
    paste ("DELETE
           FROM SeriesCatalog
           WHERE     (SeriesCatalog.SiteID IN (", paste(SiteID_, collapse = ","), "))
           AND (SeriesCatalog.VariableID IN (", paste(VariableID_, collapse = ","), "))
           AND (SeriesCatalog.MethodID IN (", paste(MethodID_, collapse = ","), "))
           AND (SeriesCatalog.QualityControlLevelID IN (", paste(QualityControlLevelID_, collapse = ","), "))
           AND ((SeriesCatalog.BeginDateTime > '", startDate, "')
           AND (SeriesCatalog.EndDateTime < '", endDate, "'))", sep = "")
  })
  return(Data)
}
