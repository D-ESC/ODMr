#'Load data to an ODM database
#'
#'The function ODMload can be used to get data into an ODM database. ODMload
#'takes a dataframe containing the required columns for working with ODM data
#'and loads it. The dataframes will either have been acquired using ODMselect
#'or created using ODM create.
#'
#'A standard SQL query is issued to the ODM database and the values are uploaded
#'in 100 row chunks. QClevel 1 data is handled by default but data containing
#'any QClevel data can be used by setting the argument to the appropriate value.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param Dataframe containing the required columns
#'@param QCcheck must match the quality control level of the data to be loaded
#'
#'@examples
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Query the database
#'tmp <- ODMselect(ODM, SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
#'
#'# Load values back to ODM
#'ODMload(ODM, Data = tmp, QCcheck = 0)
#'
#'@import RODBC
#'@import progress
#'@import dplyr
#'@export
#'@name ODMload

require(dplyr)

ODMload <- function(channel, Data, QCcheck = 1) {
  stopifnot(QCcheck == Data$QualityControlLevelID)

  DS <- ODMr:::ODMsummary(channel, Data)
  Catalog <- ODMr::ODMgetCatalog(channel) %>%
    filter(SiteID == DS$SiteID,
      VariableID == DS$VariableID,
      MethodID == DS$MethodID,
      QualityControlLevelID == DS$QualityControlLevelID)

  ValueID <- "NULL"
  Data[is.na(Data)] <- "NULL"
  chunk <- 100
  if (nrow(Data) < 100) {
    chunk <- 1
  }
  Data <- suppressWarnings(split(Data, 1:round(nrow(Data) / chunk)))
  pb <- progress::progress_bar$new(total = length(Data))
  mergeSQL <- function(x){
    Q2 <- with(x,
      paste("(",
        ValueID, ",",
        DataValue, ",",
        "NULL", ",",
        "'", LocalDateTime, "'", ",",
        UTCOffset, ",",
        "'", LocalDateTime - (60 * 60 * (as.numeric(UTCOffset))), "'", ",",
        SiteID, ",",
        VariableID, ",",
        "NULL", ",",
        "NULL", ",",
        "'", "nc", "'", ",",
        QualifierID, ",",
        MethodID, ",",
        SourceID, ",",
        "NULL", ",",
        "NULL", ",",
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
    success <- RODBC::sqlQuery(channel, {
      Q
    })
    if(is.character(success))
    {
      stop(paste(success, collapse = "\n"))
    }
    pb$tick()
    return(success)
  }

  success_summary <- dplyr::bind_rows(lapply(Data, mergeSQL))
  success_summary <- success_summary  %>%
    dplyr::group_by(Action = `$action`) %>%
    dplyr::summarise(Count = nrow(.)) %>%
    as.data.frame()
  INSERTS <- success_summary %>%
    filter(Action == "INSERT") %>%
    select(Count)
  if(nrow(Catalog) == 0) {
    Catalog <- DS
  } else if(nrow(INSERTS) > 0) {
    Catalog$EndDateTime <- DS$EndDateTime
    Catalog$ValueCount <- Catalog$ValueCount + INSERTS$Count
  } else if(DS$EndDateTime > Catalog$EndDateTime) {
    Catalog$EndDateTime <- DS$EndDateTime
  }
  Catalog <- data.frame(lapply(Catalog, gsub, pattern = "'", replacement = " "))
  SQL = ODMr:::sqlmerge(Catalog, TableName = "SeriesCatalog",
    By = c("SiteID", "VariableID", "MethodID", "QualityControlLevelID"),
    Key = "SeriesID")
  RODBC::sqlQuery(channel, SQL)
  return(success_summary)
}
