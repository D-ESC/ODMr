#'Load data to an ODM database
#'
#'The function ODMload can be used to get data into the DataValues table in an
#'ODM database. ODMload takes a dataframe containing the required columns for
#'working with ODM data and loads it. The dataframes will either have been
#'acquired using ODMselect or created using ODMcreate.
#'
#'A standard SQL query is issued to the ODM database and the values are uploaded
#'in 1000 row chunks. QClevel 1 data is handled by default but data containing
#'any QClevel data can be used by setting the argument to the appropriate value.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param Data containing the required columns
#'@param QCcheck must match the quality control level of the data to be loaded
#'@param batch_size data is sent in small chunks to the database. This setting decides the size of those chunks.
#'@param check_before_load the function will let you know what data you are about to load to the database. You can turn this behavior off by setting this to FALSE.
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
#'tmp <- ODMselect(SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
#'
#'# Load values back to ODM
#'ODMload(Data = tmp, QCcheck = 0)
#'}
#'
#'@export
#'@name ODMload

ODMload <- function(Data, QCcheck = 1, channel = ODM, batch_size = 1000, check_before_load = TRUE) {
  stopifnot(QCcheck %in% Data$QualityControlLevelID)

  DS <- ODMsummary(Data, channel)

  Catalog <- ODMgetCatalog(channel) %>%
    dplyr::filter(SiteID == DS$SiteID,
                  VariableID == DS$VariableID,
                  MethodID == DS$MethodID,
                  QualityControlLevelID == DS$QualityControlLevelID,
                  SourceID == DS$SourceID)

  if (check_before_load) {
    cat("loading: ", DS$SiteCode, DS$VariableCode, DS$MethodDescription, "\n")
    if(nrow(Catalog) >= 1)
      if(DS$BeginDateTime %within% lubridate::interval(Catalog$BeginDateTime, Catalog$EndDateTime) |
         DS$EndDateTime %within% lubridate::interval(Catalog$BeginDateTime, Catalog$EndDateTime)) {
        cat("warning:  data values already exist for this time interval")}
    question1 <- readline("Would you like to proceed? (Y/N) ")
    stopifnot(regexpr(question1, 'y', ignore.case = TRUE) == 1)}

  chunk <- batch_size
  if (nrow(Data) < chunk) {
    chunk <- 1
  }

  Data$LocalDateTime = lubridate::force_tz(Data$LocalDateTime, "UTC")

  Data <- suppressWarnings(split(Data, 1:round(nrow(Data) / chunk)))
  pb <- progress::progress_bar$new(total = length(Data))

  mergeSQL <- function(x){
    if ("ValueID" %in% names(x) & !anyNA(x$ValueID)) {
      success <- sqlmerge(x, TableName = "DataValues",
                          By = "ValueID",
                          Key = "ValueID",
                          channel = channel)
    } else {
      success <- sqlmerge(x, TableName = "DataValues",
                          By = c("LocalDateTime", "SiteID", "VariableID", "MethodID",
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

  out <- dplyr::bind_rows(lapply(Data, mergeSQL))

  if(nrow(Catalog) == 0) {
    Catalog <- DS} else {
      Catalog$BeginDateTime = min(c(Catalog$BeginDateTime, DS$BeginDateTime))
      Catalog$EndDateTime = max(c(Catalog$EndDateTime, DS$EndDateTime))
    }

  Catalog <- data.frame(lapply(Catalog, gsub, pattern = "'", replacement = " "))
  sqlmerge(Catalog, TableName = "SeriesCatalog",
           By = c("SiteID", "VariableID", "MethodID",
                  "QualityControlLevelID", "SourceID"),
           Key = "SeriesID",
           channel = channel)

  return(out)
}
