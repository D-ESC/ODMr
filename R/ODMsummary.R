#'Summarise data formated for use with the ODM
#'
#'The function ODMsummary can be used to verify the proper structure and use of
#'ID variables. Will return a meta data summary consistent with the
#'SeriesCatalog in the database.
#'
#'@param channel connection handle as returned by odbcConnect
#'@param Data containing the data in a format consistent with the ODM data model
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Return meta data for the data file
#'ODMsummary(ODM, HP4)
#'}
#'
#'@export
#'@name ODMsummary

ODMsummary <- function(channel, Data) {

  Sites <- ODMgetSites(channel)
  Variables <- ODMgetVariables(channel)
  Methods <- ODMgetMethods(channel)
  Sources <- ODMgetSource(channel)
  QCLevels <- ODMgetQCLevel(channel)


  if (!is.data.frame(Data))
    stop("Needs to be a data frame.")
  if (nrow(Data) < 1)
    stop("No data.")
  if (!("DataValue" %in% colnames(Data)))
    stop("Data values should be in a column named 'DataValue'.")
  if (!is.numeric(Data$DataValue))
    stop("DataValue should be numeric")
  if (!("LocalDateTime" %in% colnames(Data)))
    stop("Date and time should be in a column named 'LocalDateTime'.")
  if (!lubridate::is.POSIXt(Data$LocalDateTime))
    stop("LocalDateTime should be class POSIXlt or POSIXct.")
  if (anyNA(Data$LocalDateTime))
    stop("LocalDateTime should not contain any NA values.")
  if (!("SiteID" %in% colnames(Data)))
    stop("A site ID referenced from the 'Sites' table should
      be in a column named 'SiteID'.")
  if (!is.integer(Data$SiteID))
    stop("SiteID should be an integer value.")
  if (length(setdiff(Data$SiteID, Sites$SiteID) > 0 ))
    stop("SiteID does not exist in Sites table.")
  if (!("VariableID" %in% colnames(Data)))
    stop("A variable ID referenced from the 'Variables' table should
      be in a column named 'VariableID'.")
  if (!is.integer(Data$VariableID))
    stop("VariableID should be an integer value.")
  if (length(setdiff(Data$VariableID, Variables$VariableID) > 0 ))
    stop("VariableID does not exist in 'Variables' table.")
  if (!("MethodID" %in% colnames(Data)))
    stop("A method ID referenced from the 'Methods' table should
      be in a column named 'MethodID'.")
  if (!is.integer(Data$MethodID))
    stop("MethodID should be an integer value.")
  if (length(setdiff(Data$MethodID, Methods$MethodID) > 0 ))
    stop("MethodID does not exist in 'Methods' table.")
  if (!("QualityControlLevelID" %in% colnames(Data)))
    stop("A quality control level ID referenced from the 'QualityControlLevels'
      table should be in a column named 'QualityControlLevelID'.")
  if (!is.integer(Data$QualityControlLevelID))
    stop("QualityControlLevelID should be an integer value.")
  if (length(setdiff(Data$QualityControlLevelID,
    QCLevels$QualityControlLevelID) > 0 ))
    stop("QualityControlLevelID does not exist in the
      'QualityControlLevels' table.")
  if (!("SourceID" %in% colnames(Data)))
    stop("A source ID referenced from the 'Sources' table should be in a
      column named 'SourceID'.")
  if (!is.integer(Data$SourceID))
    stop("SourceID should be an integer value.")
  if (length(setdiff(Data$SourceID, Sources$SourceID) > 0 ))
    stop("SourceID does not exist in 'Sources' table.")
  if (!("UTCOffset" %in% colnames(Data)))
    stop("A UTC offset needs to be defined in a column named 'UTCOffset'.")
  if (Data$UTCOffset[1] < -12 | Data$UTCOffset[1] > 12)
    stop("Invalid UTCOffset. Value should be between -12 and 12.")

DataSeries <- Data %>%
  dplyr::select(UTCOffset, SiteID, VariableID, MethodID, SourceID,
    QualityControlLevelID) %>%
  dplyr::distinct()
N <-  names(ODMgetCatalog(channel))
DataSeries <- DataSeries %>%
  dplyr::left_join(ODMgetSites(channel, .$SiteID), by = "SiteID") %>%
  dplyr::left_join(ODMgetVariables(channel, .$VariableID), by = "VariableID") %>%
  dplyr::left_join(ODMgetMethods(channel, .$MethodID), by = "MethodID") %>%
  dplyr::left_join(ODMgetSource(channel, .$SourceID), by = "SourceID") %>%
  dplyr::left_join(ODMgetQCLevel(channel, .$QualityControlLevelID),
    by = "QualityControlLevelID") %>%
  dplyr::mutate(VariableUnitsName = ODMgetUnits(channel, .$VariableUnitsID)$UnitsName) %>%
  dplyr::mutate(TimeUnitsName = ODMgetUnits(channel, .$TimeUnitsID)$UnitsName) %>%
  dplyr::mutate(BeginDateTime = min(Data$LocalDateTime)) %>%
  dplyr::mutate(EndDateTime = max(Data$LocalDateTime)) %>%
  dplyr::mutate(BeginDateTimeUTC = BeginDateTime -
      (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
  dplyr::mutate(EndDateTimeUTC = EndDateTime -
      (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
  dplyr::mutate(ValueCount = length(Data$DataValue))  %>%
  .[, N[which(N %in% colnames(.))]]
DataSeries
}
