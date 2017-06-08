# getODMcatalog
# Pass nothing - get a list of all datasets
# Pass a SeriesID - get a single entry

# getODMstations
# Pass nothing - returns a full list of stations
# Pass a MethodID - returns a single station
# Pass a MethodCode - returns a single station

# getODMmethods
# Pass nothing - returns a full list of method objects
# Pass a MethodID - returns a single method object
# Pass a MethodCode - returns a single method object

# getODMvariables
# Pass nothing - returns full list of variable objects
# Pass a VariableID - returns a single variable object
# Pass a VariableCode - returns a single variable object

ODMgetCatalog <- function(channel = ODM, ID = NA) {
  tmp <- RODBC::sqlFetch(channel, "SeriesCatalog")
  if(!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  return(tmp)
}

ODMgetSites <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Sites")
  if(!is.na(ID)) tmp <- subset(tmp, SitesID == ID)
  if(!is.na(Code)) tmp <- subset(tmp, SitesCode == Code)
  return(tmp)
}

ODMgetMethods <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Methods")
  if(!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  if(!is.na(Code)) tmp <- subset(tmp, VariableCode == Code)
  return(tmp)
}

ODMgetVariables <-  function(channel = ODM, ID = NA, Code = NA) {
  tmp <- RODBC::sqlFetch(channel, "Variables")
  if(!is.na(ID)) tmp <- subset(tmp, VariableID == ID)
  if(!is.na(Code)) tmp <- subset(tmp, VariableCode == Code)
  return(tmp)
}
