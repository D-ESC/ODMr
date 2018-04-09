#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetCatalog <- function(channel = ODM) {
  tmp <- channel %>% dplyr::tbl("SeriesCatalog") %>% dplyr::collect()
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetSites <-  function(ID = NA, Code = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Sites") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, SiteID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, SiteCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetMethods <-  function(ID = NA, Code = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Methods") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, MethodID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, MethodCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param Code code value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetVariables <-  function(ID = NA, Code = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Variables") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  if (!is.na(Code)) tmp <- subset(tmp, VariableCode == Code)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetUnits <-  function(ID = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Units") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, UnitsID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetSource <-  function(ID = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Sources") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, SourceID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetQCLevel <-  function(ID = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("QualityControlLevels") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, QualityControlLevelID == ID)
  return(tmp)
}

#'ODM utility functions
#'
#'Set of utility functions to retrieve meta data from an ODM database
#'
#'ODMgetCatalog()
#'Pass nothing - get a list of all datasets
#'Pass a SeriesID - get a single entry
#'
#'@param ID index value for the specified object type
#'@param channel connection handle as returned by odbcConnect (default value = ODM)
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
#'# Get a list of sites in the database
#'ODMgetSites()
#'
#'# Get a list of data series in the database
#'ODMgetCatalog()
#'}
#'
#'@export

ODMgetQualifiers <-  function(ID = NA, channel = ODM) {
  tmp <- channel %>% dplyr::tbl("Qualifiers") %>% dplyr::collect()
  if (!is.na(ID)) tmp <- subset(tmp, QualifierID == ID)
  return(tmp)
}
