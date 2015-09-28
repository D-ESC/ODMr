#'Edit and flag data
#'
#'The function editData can be used to edit and flag data imported using the
#'ODMSelect function. A subset of values can be targeted and worked on. The
#'format must left-specified with respect to the standard ISO:8601 time format
#'“CCYY-MM-DD HH:MM:SS”. It is also possible to explicitly request a range of
#'times via this index-based subsetting, using the ISO-recommended “/” as the
#'range separator. The basic form is “from/to”, where both from and to are
#'optional.
#'
#'The function supplied (FUN) will be applied across the values defined by the
#'date range given to the Subset argument. The full data set will be returned
#'with the edits applied and values flagged. The default flag applied is 106
#'for 'edited' data but any flag from the value qualifiers table may be used.
#'
#'@param Data xts object returned by ODMSelect
#'@param Subset	date range to apply edit
#'@param col.name name of the column to edit
#'@param Flag flag to apply to edited data record
#'@param FUN the function to be applied
#'
#'@examples
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
#'
#'@export

editData <- function(Data, Subset = 1:nrow(Data), col.name = "DataValue",
  flag = as.integer(106), FUN = function(x) x, ...)
{
  Data[Subset, col.name] <- FUN(Data[Subset, col.name], ...)
  Data[Subset, "QualifierID"] <- flag
  return(Data)
}
