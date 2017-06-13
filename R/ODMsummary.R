require(dplyr)

ODMsummary <- function(channel = ODM, Data) {
DataSeries <- Data %>%
  select(UTCOffset, SiteID, VariableID, MethodID, SourceID, QualityControlLevelID) %>%
  distinct()
N <-  names(ODMgetCatalog(channel))
DataSeries <- DataSeries %>%
  left_join(ODMgetSites(channel, .$SiteID), by = "SiteID") %>%
  left_join(ODMgetVariables(channel, .$VariableID), by = "VariableID") %>%
  left_join(ODMgetMethods(channel, .$MethodID), by = "MethodID") %>%
  left_join(ODMgetSource(channel, .$SourceID), by = "SourceID") %>%
  left_join(ODMgetQCLevel(channel, .$QualityControlLevelID), by = "QualityControlLevelID") %>%
  mutate(VariableUnitsName = ODMgetUnits(channel, .$VariableUnitsID)$UnitsName) %>%
  mutate(TimeUnitsName = ODMgetUnits(channel, .$TimeUnitsID)$UnitsName) %>%
  mutate(BeginDateTime = min(Data$LocalDateTime)) %>%
  mutate(EndDateTime = max(Data$LocalDateTime)) %>%
  mutate(BeginDateTimeUTC = BeginDateTime - (60 * 60 * (as.numeric(UTCOffset)))) %>%
  mutate(EndDateTimeUTC = EndDateTime - (60 * 60 * (as.numeric(UTCOffset)))) %>%
  mutate(ValueCount = length(Data$DataValue))  %>%
  .[,N[which(N %in% colnames(.))]]
DataSeries
}
