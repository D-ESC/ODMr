#'Load data to an ODM database
#'
#'The function odm_write can be used to get data into the DataValues table in an
#'ODM database. odm_write takes a dataframe containing the required columns for
#'working with ODM data and loads it. The dataframes will either have been
#'acquired using odm_read or created using odm_data.
#'
#'A standard SQL query is issued to the ODM database and the values are uploaded
#'in 1000 row chunks. QClevel 1 data is handled by default but data containing
#'any QClevel data can be used by setting the argument to the appropriate value.
#'
#'@param Data containing the required columns
#'@param qc_check must match the quality control level of the data to be loaded
#'@param channel connection handle as returned by odbcConnect
#'@param batch_size data is sent in small chunks to the database. This setting
#'decides the size of those chunks.
#'@param check_before_load will let you know what data you are about to load to
#'the database. You can turn this behavior off by setting this to FALSE.
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
#'  UID = "update", PWD = rstudioapi::askForPassword("Database password"),
#'  Port = 1433)
#'
#'# Query the database
#'tmp <- odm_read(site_id = 1, variable_id = 1, method_id = 9,
#'  level_id = 0, start_date = "2013-06-01", end_date = "2013-07-01")
#'
#'# Load values back to ODM
#'odm_write(Data = tmp, qc_check = 0)
#'}
#'
#'@export
#'@name odm_write

odm_write <- function(Data, qc_check = 1, channel = ODM, batch_size = 1000,
                    check_before_load = TRUE) {
  if(!(any(Data$QualityControlLevelID %in% qc_check)))
    stop("Quality control level check failed. Double check your data and ensure you
         want to write it to the database and then set argument qc_check to the
         appropriate value.")
  if("Aggregated" %in% names(Data))
    stop("Writing aggregated values back to the database could destroy original
         values. If this is what you want to do then remove the column
         named aggregated")
  is_valid_odm(Data, channel = channel)

  ds <- odm_summary(Data, channel)

  catalog <- odm_read_tbl(channel = channel) %>%
    dplyr::filter(SiteID == ds$SiteID,
                  VariableID == ds$VariableID,
                  MethodID == ds$MethodID,
                  QualityControlLevelID == ds$QualityControlLevelID,
                  SourceID == ds$SourceID)

  if (check_before_load) {
    cat("loading: ", ds$SiteCode, ds$VariableCode, ds$MethodDescription, "\n")
    if (nrow(catalog) >= 1)
      if (ds$BeginDateTime %within%
         lubridate::interval(catalog$BeginDateTime, catalog$EndDateTime) |
         ds$EndDateTime %within%
         lubridate::interval(catalog$BeginDateTime, catalog$EndDateTime)) {
        cat("warning:  data values already exist for this time interval")
        }
    question1 <- readline("Would you like to proceed? (Y/N) ")
    stopifnot(regexpr(question1, "y", ignore.case = TRUE) == 1)
    }

  chunk <- batch_size
  if (nrow(Data) < chunk) {
    chunk <- nrow(Data)
  }

  Data$LocalDateTime <- lubridate::force_tz(Data$LocalDateTime, "UTC")

  Data <- suppressWarnings(split(Data, 1:round(nrow(Data) / chunk)))
  pb <- progress::progress_bar$new(total = length(Data))

  merge_sql <- function(x) {
    if ("ValueID" %in% names(x) & !anyNA(x$ValueID)) {
      success <- sqlmerge(x, TableName = "DataValues",
                          By = "ValueID",
                          Key = "ValueID",
                          channel = channel)
    } else {
      success <- sqlmerge(x, TableName = "DataValues",
                          By = c("LocalDateTime", "SiteID",
                                 "VariableID", "MethodID",
                                 "QualityControlLevelID", "SourceID"),
                          Key = "ValueID",
                          channel = channel)
    }
    if (is.character(success)) {
      stop(paste(success, collapse = "\n"))
    }
    pb$tick()
    return(success)
  }

  out <- dplyr::bind_rows(lapply(Data, merge_sql))

  SQL <- glue::glue(
    "EXEC dbo.spUpdateSeriesIncatalog
    @SiteID = {ds$SiteID},
    @VariableID = {ds$VariableID},
    @MethodID = {ds$MethodID},
    @QualityControlLevelID = {ds$QualityControlLevelID};"
  )

  success <- DBI::dbExecute(channel,
                            SQL)

  return(out)
}
