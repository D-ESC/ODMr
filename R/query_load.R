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
#'@param CatalogID index value for a specific record within the series catalog
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
#'\dontrun{ODM <- odbcConnect("Connection", "User id", "Password")
#'Data <- ODMSelect(ODM, CatalogID = 10, "2013-06-01", "2014-06-01")}
#'
#'@export

ODMselect <- function(channel, CatalogID = NULL, SiteID = NULL,
  VariableID = NULL, MethodID = NULL, QCLevelID = 1,
  startDate = "1970-01-1 00:00:00", endDate = Sys.Date())
{
  Catalog = RODBC::sqlFetch(channel, "SeriesCatalog")
  Data <- RODBC::sqlQuery(channel, {
    paste ("SELECT DataValues.ValueID,DataValues.DataValue,
            DataValues.LocalDateTime,DataValues.UTCOffset,
            DataValues.SiteID,DataValues.VariableID,
            DataValues.QualifierID,DataValues.MethodID,
            DataValues.SourceID,DataValues.QualityControlLevelID
          FROM OD.dbo.DataValues DataValues
          WHERE     (DataValues.SiteID = ",
      if (!is.null(CatalogID ))
        Catalog[CatalogID, "SiteID"] else SiteID,")
            AND (DataValues.VariableID = ",
      if (!is.null(CatalogID ))
        Catalog[CatalogID, "VariableID"] else VariableID,")
            AND (DataValues.MethodID = ",
      if (!is.null(CatalogID ))
        Catalog[CatalogID, "MethodID"] else MethodID,")
            AND (DataValues.QualityControlLevelID = ",
      if (!is.null(CatalogID ))
        Catalog[CatalogID, "QualityControlLevelID"] else QCLevelID,")
            AND ((DataValues.LocalDateTime > '", startDate,"')
            AND (DataValues.LocalDateTime < '", endDate,"'))
          ORDER BY DataValues.LocalDateTime ASC", sep = "")
  })
  Data <- xts::xts(Data[, -3], order.by = Data[, 3])
  return(Data)
}

#'Load data to an ODM database
#'
#'The function ODMload can be used to get data into an ODM database. ODMload
#'takes an xts object containing the required columns for working with ODM data
#'and loads it. The xts objects will either have been acquired using ODMselect
#'or created using ODM create.
#'
#'A standard SQL query is issued to the ODM database and the values are uploaded
#'in 100 row chunks. QClevel 1 data is handled by default but data containing
#'any QClevel data can be used by setting the argument to the appropriate value.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param obj xts object containing the appropriate data
#'@param QCcheck must match the quality control level of the data to be loaded
#'
#'@examples
#'\dontrun{ODM <- odbcConnect("Connection", "User id", "Password")
#'ODMload(ODM, "HP4_stage_corr")}
#'
#'@export

ODMload <- function(channel, obj, QCcheck = 1)
{
  stopifnot(QCcheck == obj$QualityControlLevelID)
  ValueID = "NULL"
  zoo::coredata(obj)[is.na(obj)] <- "NULL"
  chunk <- 100
  if (nrow(obj) < 100)
  {
    chunk <- nrow(obj)
  }
  Data <- split(obj, 1:round(nrow(obj)/chunk))
  mergeSQL <- function(x)
  {
    Q2 <- with(data.frame(LocalDateTime = zoo::index(x), zoo::coredata(x)),
      paste("(",
        ValueID,",",
        DataValue,",",
        "NULL",",",
        "'",LocalDateTime,"'",",",
        UTCOffset,",",
        "'",LocalDateTime - (60*60*(as.numeric(UTCOffset))),"'",",",
        SiteID,",",
        VariableID,",",
        "NULL",",",
        "NULL",",",
        "'","nc","'",",",
        QualifierID,",",
        MethodID,",",
        SourceID,",",
        "NULL",",",
        "NULL",",",
        QualityControlLevelID,
        ")", sep = ""))
    Q1 <- "MERGE DataValues AS T USING (VALUES"
    Q2 <- paste(Q2, collapse = ",")
    Q3 <- ") AS S(ValueID, DataValue, ValueAccuracy, LocalDateTime, UTCOffset,
          DateTimeUTC, SiteID, VariableID, OffsetValue, OffsetTypeID,
          CensorCode, QualifierID, MethodID, SourceID, SampleID, DerivedFromID,
          QualityControlLevelID)
      ON (T.LocalDateTime = S.LocalDateTime AND T.SiteID = S.SiteID AND
          T.VariableID = S.VariableID AND T.MethodID = S.MethodID AND
          T.QualityControlLevelID = S.QualityControlLevelID)
      WHEN NOT MATCHED THEN INSERT (DataValue, ValueAccuracy, LocalDateTime,
          UTCOffset, DateTimeUTC, SiteID, VariableID, OffsetValue,
          OffsetTypeID, CensorCode, QualifierID, MethodID, SourceID, SampleID,
          DerivedFromID, QualityControlLevelID)
      VALUES (S.DataValue, S.ValueAccuracy, S.LocalDateTime, S.UTCOffset,
          S.DateTimeUTC, S.SiteID, S.VariableID, S.OffsetValue, S.OffsetTypeID,
          S.CensorCode, S.QualifierID, S.MethodID, S.SourceID, S.SampleID,
          S.DerivedFromID, S.QualityControlLevelID)
      WHEN MATCHED THEN UPDATE SET T.DataValue = S.DataValue,
          T.QualifierID = S.QualifierID
      OUTPUT $action, inserted.ValueID;"
    Q <- paste(Q1, Q2, Q3)
    RODBC::sqlQuery(channel, {
      Q
    })
  }
  lapply(Data, mergeSQL)
}
