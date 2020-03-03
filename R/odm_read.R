#'Query an ODM database
#'
#'The function odm_read can be used to get data from ODM database. ODMselect
#'returns a data frame containing the required columns for working with ODM
#'data.
#'
#'A standard SQL query is issued to the ODM database and the values are
#'returned. The data returned can be limited by a start
#'and/or end date defined by the user. Smaller datasets can speed up the
#'workflow especially when visualising data.
#'
#'@param utc_offset of the observation
#'@param site_id index value for the location at which the observation was made
#'@param variable_id index value for the variable that the data represents
#'@param qualifier_id of the observations you want to extract
#'@param method_id index value for the method used to collect the observation
#'@param level_id the level of quality control processing
#'@param source_id the source of the data
#'@param aggregate_by aggregation level to include in SQL query. Either hour,
#'day, month, year or none
#'@param FUN function to aggregate by. Either min, max, mean, or sum
#'@param start_date access data from this date forward
#'@param end_date access data up to this date
#'@param n maximum number of observations to return
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
#'Data <- odm_read(site_id = 1, variable_id = 1, method_id = 9,
#'  level_id = 0, start_date = "2013-06-01",
#'  end_date = "2013-07-01", channel = ODM)
#'}
#'
#'@export

odm_read <- function(utc_offset = -5,
                     site_id = NULL,
                     variable_id = NULL,
                     qualifier_id = NULL,
                     method_id = NULL,
                     source_id = NULL,
                     level_id = NULL,
                     aggregate_by = "day",
                     FUN = "mean",
                     start_date = NULL,
                     end_date = NULL,
                     n = 1000000,
                     channel = ODM) {

  choices <- c(
    "min" = "min(DataValue, na.rm = TRUE)",
    "max" = "max(DataValue, na.rm = TRUE)",
    "mean" = "mean(DataValue, na.rm = TRUE)",
    "sum" = "sum(DataValue, na.rm = TRUE)"
  )

  Old.TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "Etc/GMT")

  if (is.null(site_id) & is.null(variable_id) & is.null(method_id)) {
    series <- odm_read_tbl(odm_tbl = "SeriesCatalog", channel = channel)[1, ]
    site_id <- series$SiteID
    variable_id <- series$VariableID
    method_id <- series$MethodID
    level_id <- series$QualityControlLevelID
  }

  result <- channel %>%
    dplyr::tbl("DataValues") %>%
    {if (!is.null(site_id)) dplyr::filter(., SiteID %in% site_id)
      else .} %>%
    {if (!is.null(variable_id)) dplyr::filter(., VariableID %in% variable_id)
      else .} %>%
    {if (!is.null(method_id)) dplyr::filter(., MethodID %in% method_id)
      else .} %>%
    {if (!is.null(level_id)) dplyr::filter(., QualityControlLevelID %in% level_id)
      else .} %>%
    {if (!is.null(source_id)) dplyr::filter(., SourceID %in% source_id)
      else .} %>%
    dplyr::group_by(UTCOffset,
                    SiteID,
                    VariableID,
                    MethodID,
                    SourceID,
                    QualityControlLevelID)

  if (!is.null(start_date)) {
    result <- result %>%
      dplyr::filter(LocalDateTime > start_date)
  }

  if (!is.null(end_date)) {
    result <- result %>%
      dplyr::filter(LocalDateTime < end_date)
  }

  if (aggregate_by == "hour") {
    result <- result %>%
      dplyr::group_by(LocalDateTime =
                        DATEADD(HOUR, DATEDIFF(HOUR, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (aggregate_by == "day") {
    result <- result %>%
      dplyr::group_by(LocalDateTime =
                        DATEADD(DAY, DATEDIFF(DAY, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (aggregate_by == "month") {
    result <- result %>%
      dplyr::group_by(LocalDateTime =
                        DATEADD(MONTH, DATEDIFF(MONTH, 0, LocalDateTime), 0),
                      add = TRUE)
  }
  if (aggregate_by == "none") {
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
      dplyr::collect(n = n)
  }
  if (aggregate_by != "none") {
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
      dplyr::mutate(Aggregated = paste(aggregate_by, FUN)) %>%
      dplyr::arrange(LocalDateTime,
                     DataValue,
                     SiteID,
                     VariableID,
                     QualifierID,
                     MethodID,
                     SourceID,
                     QualityControlLevelID) %>%
      dplyr::collect(n = n)
  }
  if (exists("results")) {
    result$LocalDateTime <-
      lubridate::force_tz(result$LocalDateTime,
                          if (result$UTCOffset[1] < 0) {
                            gsub("!", -result$UTCOffset[1], "Etc/GMT+!")
                          } else {
                            gsub("!", result$UTCOffset[1], "Etc/GMT!")
                          }
      )}
  result <- result %>% dplyr::ungroup()
  Sys.setenv(TZ = Old.TZ)
  result
}
