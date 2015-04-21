#'Query an ODM database
#'
#'The function ODMselect can be used to get data from ODM database. ODMselect
#'returns an xts object containing required columns for working with ODM data.
#'The xts object contains an array of values comprising your data in matrix form
#'and an index based on the LocalDateTime.
#'
#'A standard SQL query is issued to the ODM database and the values are
#'returned. Output is an xts object. The data returned can be limited by a start
#'and/or end date defined by the user. Smaller datasets can speed up the
#'workflow especially when visualising data.
#'
#'A specific series from the catalog may be specified using the series catalog
#'ID. Since the series catalog is not stable through time you may also query the
#'database by specifying SiteID, VariableID and MethodID.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param SeriesID index value for a specific record within the series catalog
#'@param SiteID index value for the location at which the observation was made
#'@param VariableID index value for the variable that the data represents
#'@param MethodID index value for the method used to collect the observation
#'@param QCLevelID the level of quality control processing
#'@param startDate access data from this date forward
#'@param endDate access data up to this date
#'
#'@return An xts object
#'
#'@examples
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Extract data by SeriesID
#'tmp <- ODMselect(ODM, SeriesID = 10, startDate = "2013-06-01",
#'  endDate = "2013-07-01")
#'
#'# Extract data by site, variable, method and QC level. Clear and consistent
#'# method to extract data.
#'tmp <- ODMselect(ODM, SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
#'
#'# Extract multiple data series.
#'tmp = ODMselect(ODM, SiteID = c(1,5) , VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
#'
#'@import RODBC
#'@export

ODMselect <- function(channel, SeriesID = NULL, SiteID = NULL,
  VariableID = NULL, MethodID = NULL, QCLevelID = 1,
  startDate = "1970-01-1 00:00:00", endDate = Sys.Date())
{
  Old.TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "Etc/GMT")
  Catalog <- RODBC::sqlFetch(channel, "SeriesCatalog")
  Data <- RODBC::sqlQuery(channel, {
    paste ("SELECT DataValues.ValueID,DataValues.DataValue,
            DataValues.LocalDateTime,DataValues.UTCOffset,
            DataValues.SiteID,DataValues.VariableID,
            DataValues.QualifierID,DataValues.MethodID,
            DataValues.SourceID,DataValues.QualityControlLevelID
          FROM OD.dbo.DataValues DataValues
          WHERE     (DataValues.SiteID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "SiteID"], collapse = ",") else
          paste(SiteID, collapse = ","),"))
            AND (DataValues.VariableID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "VariableID"], collapse = ",") else
          paste(VariableID, collapse = ","),"))
            AND (DataValues.MethodID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "MethodID"], collapse = ",") else
          paste(MethodID, collapse = ","),"))
            AND (DataValues.QualityControlLevelID IN (",
      if (!is.null(SeriesID ))
        paste(Catalog[SeriesID, "QualityControlLevelID"], collapse = ",") else
          paste(QCLevelID, collapse = ","),"))
            AND ((DataValues.LocalDateTime > '", startDate,"')
            AND (DataValues.LocalDateTime < '", endDate,"'))
          ORDER BY DataValues.SiteID ASC, DataValues.VariableID ASC,
            DataValues.MethodID ASC, DataValues.QualityControlLevelID ASC,
            DataValues.LocalDateTime ASC", sep = "")
  })
  Data <- xts::xts(Data[, -3], order.by = Data[, 3])
  zoo::index(Data) = lubridate::force_tz(zoo::index(Data),
    if(-Data$UTCOffset[1] > 0)
      gsub("!", -Data$UTCOffset[1], "Etc/GMT+!") else
        gsub("!", -Data$UTCOffset[1], "Etc/GMT!"))
  Sys.setenv(TZ=Old.TZ)
  return(Data)
}
