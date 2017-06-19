require(dplyr)

ODMsummary <- function(channel = ODM, Data) {

  Sites <- ODMr::ODMgetSites(channel)
  Variables <- ODMr::ODMgetVariables(channel)
  Methods <- ODMr::ODMgetMethods(channel)
  Sources <- ODMr::ODMgetSource(channel)
  QCLevels <- ODMr::ODMgetQCLevel(channel)


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
  select(UTCOffset, SiteID, VariableID, MethodID, SourceID,
    QualityControlLevelID) %>%
  distinct()
N <-  names(ODMgetCatalog(channel))
DataSeries <- DataSeries %>%
  left_join(ODMgetSites(channel, .$SiteID), by = "SiteID") %>%
  left_join(ODMgetVariables(channel, .$VariableID), by = "VariableID") %>%
  left_join(ODMgetMethods(channel, .$MethodID), by = "MethodID") %>%
  left_join(ODMgetSource(channel, .$SourceID), by = "SourceID") %>%
  left_join(ODMgetQCLevel(channel, .$QualityControlLevelID),
    by = "QualityControlLevelID") %>%
  mutate(VariableUnitsName = ODMgetUnits(channel, .$VariableUnitsID)$UnitsName) %>%
  mutate(TimeUnitsName = ODMgetUnits(channel, .$TimeUnitsID)$UnitsName) %>%
  mutate(BeginDateTime = min(Data$LocalDateTime)) %>%
  mutate(EndDateTime = max(Data$LocalDateTime)) %>%
  mutate(BeginDateTimeUTC = BeginDateTime -
      (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
  mutate(EndDateTimeUTC = EndDateTime -
      (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
  mutate(ValueCount = length(Data$DataValue))  %>%
  .[, N[which(N %in% colnames(.))]]
DataSeries
}
