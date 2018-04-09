#'Query an ODM database
#'
#'The function ODMselect can be used to get data from ODM database. ODMselect
#'returns a data frame containing the required columns for working with ODM data.
#'
#'A standard SQL query is issued to the ODM database and the values are
#'returned. The data returned can be limited by a start
#'and/or end date defined by the user. Smaller datasets can speed up the
#'workflow especially when visualising data.
#'
#'Data series from the catalog may be specified using the series catalog
#'ID. Since the series catalog is not stable through time you may also query the
#'database by specifying SiteID, VariableID and MethodID.
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
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
#'  UID = "update", PWD = rstudioapi::askForPassword("Database password"),
#'  Port = 1433)
#'
#'# Extract data by SeriesID
#'tmp <- ODMgetData(SeriesID = 10, startDate = "2013-06-01",
#'  endDate = "2013-07-01", channel = ODM)
#'
#'# Extract data by site, variable, method and QC level. Clear and consistent
#'# method to extract data.
#'tmp <- ODMgetData(SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01",
#'  channel = ODM)
#'
#'# Extract multiple data series.
#'tmp = ODMgetData(SiteID = c(1,5) , VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01",
#'  channel = ODM)
#'}
#'
#'@export

ODMgetData <- function(SeriesID = NULL, SiteID = "SiteID",
  VariableID = "VariableID", MethodID = "MethodID",
  QCLevelID = "QualityControlLevelID", startDate = "1970-01-1 00:00:00",
  endDate = Sys.Date(), channel = ODM) {
  Old.TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "Etc/GMT")
  Catalog <- ODMgetCatalog(channel = channel)
  Data <- odbc::dbGetQuery(channel, {
    paste ("SELECT DataValues.ValueID,DataValues.DataValue,
            DataValues.LocalDateTime,DataValues.UTCOffset,
            DataValues.SiteID,DataValues.VariableID,
            DataValues.QualifierID,DataValues.MethodID,
            DataValues.SourceID,DataValues.QualityControlLevelID
          FROM OD.dbo.DataValues DataValues
          WHERE     (DataValues.SiteID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "SiteID"], collapse = ",") else
          paste(SiteID, collapse = ","), "))
            AND (DataValues.VariableID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "VariableID"], collapse = ",") else
          paste(VariableID, collapse = ","), "))
            AND (DataValues.MethodID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "MethodID"], collapse = ",") else
          paste(MethodID, collapse = ","), "))
            AND (DataValues.QualityControlLevelID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "QualityControlLevelID"], collapse = ",") else
          paste(QCLevelID, collapse = ","), "))
            AND ((DataValues.LocalDateTime > '", startDate, "')
            AND (DataValues.LocalDateTime < '", endDate, "'))
          ORDER BY DataValues.SiteID ASC, DataValues.VariableID ASC,
            DataValues.MethodID ASC, DataValues.QualityControlLevelID ASC,
            DataValues.LocalDateTime ASC", sep = "")
  })

 Data$LocalDateTime <- lubridate::force_tz(Data$LocalDateTime,
    if (Data$UTCOffset[1] > 0)
      gsub("!", -Data$UTCOffset[1], "Etc/GMT!+") else
        gsub("!", Data$UTCOffset[1], "Etc/GMT!"))
  Sys.setenv(TZ = Old.TZ)
  return(Data)
}
