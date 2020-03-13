#'Create ODM formated dataframe
#'
#'The function odm_data can be used to help in formating new data. odm_data
#'takes a vector of dates and data and outputs a dataframe formated for use
#'with the functions in this package. site_id, variable_id, and method_id need
#'to already exist in their respective tables in the ODM.
#'
#'The output is a properly formated dataframe suitable for further
#'use in workflows involving this package.
#'
#'@param date_time local date time for each record
#'@param data_value matching data value for each date time record
#'@param utc_offset difference in hours from Coordinated Universal Time (UTC)
#'@param site_id corresponds to an existing Sites record in the Sites table
#'@param variable_id corresponds to an existing record in the Variables table
#'@param qualifier_id corresponds to an existing record in the Qualifiers table
#'@param method_id corresponds to an existing record in the Methods table
#'@param source_id corresponds to an existing record in the Sources table
#'@param level_id corresponds to a record in the QualityControlLevels table
#'@param channel connection handle as returned by odbcConnect
#'
#'@examples
#'
#'\dontrun{
#'# dates
#'date = seq(as.Date("2010/1/1"), as.Date("2011/1/1"), "days")
#'# dummy data values
#'value = 1:366
#'
#'# creation of dataframe that meets the requirements of the ODM.
#'tmp = odm_data(date_time = date, data_value = value, site_id = 1,
#'  variable_id = 1, method_id = 9)
#'}
#'
#'@export

odm_data <-
  function(date_time,
           data_value,
           utc_offset = -5,
           site_id,
           variable_id,
           qualifier_id = NA,
           method_id,
           source_id = 1,
           level_id = 0,
           channel = ODM) {
    if (!is.numeric(as.numeric(data_value)))
      stop("data_value needs to be convertable to type numeric.")
    if (!lubridate::is.POSIXt(date_time))
      stop("date_time should be class POSIXlt or POSIXct.")
    if (!is.integer(as.integer(site_id)))
      stop("site_id should be convertable to type integer.")
    if (!is.integer(as.integer(variable_id)))
      stop("variable_id should be convertable to type integer.")
    if (!is.integer(as.integer(qualifier_id)))
      stop("qualifier_id should be convertable to type integer.")
    if (!is.integer(as.integer(method_id)))
      stop("method_id should be convertable to type integer.")
    if (!is.integer(as.integer(level_id)))
      stop("level_id should be convertable to type integer.")
    if (!is.integer(as.integer(source_id)))
      stop("source_idshould be convertable to type integer.")
    if (!is.integer(as.integer(utc_offset[1])))
      stop("utc_offset should be convertable to type integer.")
    
    Old.TZ <- Sys.getenv("TZ")
    Sys.setenv(TZ = "Etc/GMT")
    
    if (utc_offset > 0)
      TZ <- gsub("!", abs(utc_offset), "Etc/GMT-!")
    else
      TZ <- gsub("!", abs(utc_offset), "Etc/GMT+!")
    Data <- data.frame(ValueID = rep(NA, length(data_value)))
    Data$DataValue <- as.numeric(data_value)
    Data$LocalDateTime <-
      try(lubridate::force_tz(date_time, TZ))
    Data$UTCOffset <- as.integer(utc_offset)
    Data$SiteID <- as.integer(site_id)
    Data$VariableID <- as.integer(variable_id)
    Data$QualifierID <- as.integer(qualifier_id)
    Data$MethodID <- as.integer(method_id)
    Data$SourceID <- as.integer(source_id)
    Data$QualityControlLevelID <- as.integer(level_id)

    is_valid_odm(Data, channel = channel)
    Sys.setenv(TZ = Old.TZ)
    return(Data)
  }
