#'Create ODM formated dataframe
#'
#'The function ODMcreate can be used to help in formating new data. ODMcreate
#'takes a vector of dates and data and outputs a dataframe formated for use
#'with the functions in this package. Site ID, VariableID, and MethodID need to
#'already exist in their respective tables in the ODM.
#'
#'The vectors are given with the arguments for SiteID, VariableID and MethodID.
#'The output is a properly formated dataframe suitable for further use in
#'workflows involving this package.
#'
#'@param LocalDateTime local date time for each record
#'@param DataValue matching data value for each date time record
#'@param UTCOffset difference in hours from Coordinated Universal Time (UTC)
#'@param SiteID corresponds to an existing Sites record in the Sites table
#'@param VariableID corresponds to an existing record in the Variables table
#'@param QualifierID corresponds to an existing record in the Qualifiers table
#'@param MethodID corresponds to an existing record in the Methods table
#'@param SourceID corresponds to an existing record in the Sources table
#'@param QCLevelID corresponds to a record in the QualityControlLevels table
#'
#'@examples
#'# dates
#'date = seq(as.Date("2010/1/1"), as.Date("2011/1/1"), "days")
#'# dummy data values
#'value = 1:366
#'
#'# creation of formated xts object
#'tmp = ODMcreate(LocalDateTime = date, DataValue = value, SiteID = 1,
#'  VariableID = 1, MethodID = 9)
#'
#'@export

ODMcreate <- function(LocalDateTime, DataValue, UTCOffset = -5, SiteID, VariableID,
  QualifierID = NA, MethodID, SourceID = 1, QCLevelID = 0)
{
  if(UTCOffset > 0)
    TZ = gsub("!", -UTCOffset, "Etc/GMT!+") else
      TZ = gsub("!", UTCOffset, "Etc/GMT!")
  Data <- data.frame(ValueID = rep(NA, length(DataValue)))
  Data$DataValue <- try(as.numeric(DataValue))
  Data$LocalDateTime <- try(lubridate::force_tz(anytime::anytime(LocalDateTime), TZ))
  Data$UTCOffset <- try(as.integer(UTCOffset))
  Data$SiteID <- try(as.integer(SiteID))
  Data$VariableID <- try(as.integer(VariableID))
  Data$QualifierID <- try(as.integer(QualifierID))
  Data$MethodID <- try(as.integer(MethodID))
  Data$SourceID <- try(as.integer(SourceID))
  Data$QualityControlLevelID <- try(as.integer(QCLevelID))
  return(Data)
}
