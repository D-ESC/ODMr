#'Load data to an ODM database
#'
#'The function ODMload can be used to get data into the DataValues table in an
#'ODM database. ODMload takes a dataframe containing the required columns for
#'working with ODM data and loads it. The dataframes will either have been
#'acquired using ODMselect or created using ODMcreate.
#'
#'A standard SQL query is issued to the ODM database and the values are uploaded
#'in 100 row chunks. QClevel 1 data is handled by default but data containing
#'any QClevel data can be used by setting the argument to the appropriate value.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param Data containing the required columns
#'@param QCcheck must match the quality control level of the data to be loaded
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Query the database
#'tmp <- ODMselect(ODM, SiteID = 1, VariableID = 1, MethodID = 9,
#'  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
#'
#'# Load values back to ODM
#'ODMload(ODM, Data = tmp, QCcheck = 0)
#'}
#'
#'@export
#'@name ODMload

ODMload <- function(channel, Data, QCcheck = 1) {
  stopifnot(QCcheck == Data$QualityControlLevelID)

  DS <- ODMsummary(channel, Data)
  Catalog <- ODMgetCatalog(channel) %>%
    dplyr::filter(SiteID == DS$SiteID,
      VariableID == DS$VariableID,
      MethodID == DS$MethodID,
      QualityControlLevelID == DS$QualityControlLevelID)

  Data$DateTimeUTC <- Data$LocalDateTime -
    (60 * 60 * (as.numeric(Data$UTCOffset)))
  Data$CensorCode <- "nc"

  chunk <- 100
  if (nrow(Data) < 100) {
    chunk <- 1
  }

  Data <- suppressWarnings(split(Data, 1:round(nrow(Data) / chunk)))
  pb <- progress::progress_bar$new(total = length(Data))

  mergeSQL <- function(x){
    SQL <- sqlmerge(x, TableName = "DataValues",
      By = c("LocalDateTime", "SiteID", "VariableID", "MethodID",
        "QualityControlLevelID", "SourceID"),
      Key = "ValueID")
    success <- RODBC::sqlQuery(channel, {
      SQL
    })
    if (is.character(success)) {
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
    dplyr::filter(Action == "INSERT") %>%
    dplyr::select(Count)
  if (nrow(Catalog) == 0) {
    Catalog <- DS
  } else if (nrow(INSERTS) > 0) {
    Catalog$EndDateTime <- max(c(DS$EndDateTime, Catalog$EndDateTime))
    Catalog$ValueCount <- Catalog$ValueCount + INSERTS$Count
  }
  Catalog <- data.frame(lapply(Catalog, gsub, pattern = "'", replacement = " "))
  SQL <- sqlmerge(Catalog, TableName = "SeriesCatalog",
    By = c("SiteID", "VariableID", "MethodID",
      "QualityControlLevelID", "SourceID"),
    Key = "SeriesID")
  RODBC::sqlQuery(channel, SQL)
  return(success_summary)
}
