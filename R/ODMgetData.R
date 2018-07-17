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
#'@param SiteID_ index value for the location at which the observation was made
#'@param VariableID_ index value for the variable that the data represents
#'@param MethodID_ index value for the method used to collect the observation
#'@param QualityCControlLevelID_ the level of quality control processing
#'@param AggregateBy aggregation level to include in SQL query. Either hour,
#'day, month, year or none
#'@param FUN function to aggregate by. Either min, max, mean, or sum
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
#'# Extract data by site, variable, method and QC level. Clear and consistent
#'# method to extract data.
#'tmp <- ODMgetData(SiteID = 1, VariableID = 1, MethodID = 9,
#'  QualityCcontrolLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01",
#'  channel = ODM)
#'}
#'
#'@export

ODMgetData <- function(SiteID_,
                       VariableID_,
                       MethodID_,
                       QualityControlLevelID_,
                       AggregateBy = 'day',
                       FUN = 'mean',
                       startDate = NULL,
                       endDate = NULL,
                       channel = ODM) {

  choices = c(
    'min' = 'min(DataValue, na.rm = TRUE)',
    'max' = 'max(DataValue, na.rm = TRUE)',
    'mean' = 'mean(DataValue, na.rm = TRUE)',
    'sum' = 'sum(DataValue, na.rm = TRUE)'
  )
  Old.TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "Etc/GMT")

  result <- channel %>% dplyr::tbl("DataValues") %>%
    dplyr::filter(
      SiteID %in% SiteID_,
      VariableID %in% VariableID_,
      MethodID %in% MethodID_,
      QualityControlLevelID %in% QualityControlLevelID_
    ) %>%
    dplyr::group_by(UTCOffset,
                    SiteID,
                    VariableID,
                    MethodID,
                    SourceID,
                    QualityControlLevelID)

  if (!is.null(startDate)) {
    result <- result %>%
      dplyr::filter(LocalDateTime > startDate)
  }

  if (!is.null(endDate)) {
    result <- result %>%
      dplyr::filter(LocalDateTime < endDate)
  }

  if (AggregateBy == 'hour') {
    result <- result %>%
      dplyr::group_by(LocalDateTime = DATEADD(HOUR, DATEDIFF(HOUR, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (AggregateBy == 'day') {
    result <- result %>%
      dplyr::group_by(LocalDateTime = DATEADD(DAY, DATEDIFF(DAY, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (AggregateBy == 'month') {
    result <- result %>%
      dplyr::group_by(LocalDateTime = DATEADD(MONTH, DATEDIFF(MONTH, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (AggregateBy == 'none') {
    result <- result %>%
      dplyr::select(
        ValueID,
        LocalDateTime,
        DataValue,
        UTCOffset,
        SiteID,
        VariableID,
        QualifierID,
        MethodID,
        SourceID,
        QualityControlLevelID
      ) %>%
      dplyr::collect()
  }
  if (AggregateBy != 'none') {
    result <-
      result %>%
      dplyr::summarise(
        DataValue = rlang::parse_expr(choices[FUN]),
        QualifierID = min(QualifierID, na.rm = TRUE)
      ) %>%
      dplyr::select(
        LocalDateTime,
        DataValue,
        UTCOffset,
        SiteID,
        VariableID,
        QualifierID,
        MethodID,
        SourceID,
        QualityControlLevelID
      ) %>%
      dplyr::collect() %>%
      dplyr::mutate(Aggregated = paste(AggregateBy, FUN)) %>%
      dplyr::arrange(LocalDateTime,
                     DataValue,
                     SiteID,
                     VariableID,
                     QualifierID,
                     MethodID,
                     SourceID,
                     QualityControlLevelID)
  }
  result$LocalDateTime <- lubridate::force_tz(result$LocalDateTime,
                                              if (result$UTCOffset[1] < 0){
                                                gsub("!", -result$UTCOffset[1], "Etc/GMT+!")
                                                } else {
                                                  gsub("!", result$UTCOffset[1], "Etc/GMT!")
                                                  }
                                              )
  result %>% dplyr::ungroup()
  Sys.setenv(TZ = Old.TZ)
  result
}
