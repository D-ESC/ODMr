ODMsummary <- function(channel, DF) {
  QualityControlLevels <- RODBC::sqlFetch(ODM, "QualityControlLevels")
  Methods <- RODBC::sqlFetch(channel, "Methods")
  Variables <- RODBC::sqlFetch(channel, "Variables")
  Sites <- RODBC::sqlFetch(channel, "Sites")
  tmp <- Reduce(function(x, y) merge(x, y, all=FALSE),
    list(DF[1,], Sites, Variables, Methods, QualityControlLevels))
  print(
    tmp[,c("SiteCode", "VariableCode", "Definition", "MethodDescription")])
  }
