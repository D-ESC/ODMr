#'Edit and flag data
#'
#'The function editData can be used to edit and flag data imported using the
#'ODMSelect function. A subset of values can be targeted and worked on.
#'
#'The function supplied (FUN) will be applied across the values defined by the
#'date range given to the Subset argument. The full data set will be returned
#'with the edits applied and values flagged. The default flag applied is 106
#'for 'edited' data but any flag from the value qualifiers table may be used.
#'
#'@param Dataframe returned by ODMSelect
#'@param Subset	of values to apply edit
#'@param col.name name of the column to edit
#'@param Flag flag to apply to edited data record
#'@param FUN the function to be applied
#'
#'@examples
#'
#'library(dplyr)
#'library(lubridate)
#'
#'Data = data.frame(LocalDateTime = seq(Sys.time(), length.out = 100,
#'  by = "mins"), DataValue = rnorm(100))
#'
#'# set flag
#'Data = editData(Data, flag = 102)
#'# set a constant value
#'Data = editData(Data, FUN = function (x) 5)
#'# apply offset
#'Data = editData(Data, FUN = function (x) x + 5)
#'# subset data
#'Data = editData(between(Data$LocalDateTime,
#'  ymd("2011-01-01"), ymd("2011-02-02")),
#'  FUN = function (x) x + -5)
#'
#'@export

editData <- function(Data, Subset = 1:nrow(Data), col.name = "DataValue",
  flag = as.integer(106), FUN = function(x) x, ...)
{
  Data[Subset, col.name] <- FUN(Data[Subset, col.name], ...)
  Data[Subset, "QualifierID"] <- flag
  return(Data)
}
