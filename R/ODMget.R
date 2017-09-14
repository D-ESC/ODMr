#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetCatalog <- function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "SeriesCatalog", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetSites
#'Pass nothing - returns a full list of stations
#'Pass a MethodID - returns a single station
#'Pass a MethodCode - returns a single station
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetSites <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Sites", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, SiteID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, SiteCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetMethods
#'Pass nothing - returns a full list of methods
#'Pass a MethodID - returns a single method
#'Pass a MethodCode - returns a single method
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetMethods <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Methods", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, MethodID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, MethodCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetVariables
#'Pass nothing - returns full list of variables
#'Pass a VariableID - returns a single variable
#'Pass a VariableCode - returns a single variable
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetVariables <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Variables", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, VariableCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetUnits
#'Pass nothing - returns full list of units objects
#'Pass a UnitsID - returns a single units object
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetUnits <-  function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "Units", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, UnitsID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetSource
#'Pass nothing - returns full list of sources
#'Pass a SourceID - returns a single source
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetSource <-  function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "Sources", stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, SourceID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetQCLevel
#'Pass nothing - returns full list of Quality Control Levels
#'Pass a SourceID - returns a single Quality Control Level
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetQCLevel <-  function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "QualityControlLevels",
    stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, QualityControlLevelID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetQualifiers
#'Pass nothing - returns full list of Quality Control Levels
#'Pass a SourceID - returns a single Quality Control Level
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
#'@param ID index value for the specified object type
#'
#'@return A data frame
#'
#'@examples
#'
#'\dontrun{
#'# Establish connection with database
#'ODM <- odbcConnect("ODM", "update", "update")
#'
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@import RODBC
#'@export

ODMgetQualifiers <-  function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "Qualifiers",
                         stringsAsFactors = FALSE)
  if (!is.na(ID)) tmp <- subset(tmp, QualifierID == ID)
  return(tmp)
}
