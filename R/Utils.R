#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' within operator
#'
#' @name %within%
#' @rdname within
#' @keywords internal
#' @export
#' @importFrom lubridate %within%
#' @usage lhs \%within\% rhs
NULL

sqlmerge <- function(Data, TableName, By, Key, channel = ODM) {
  if (Key %in% names(Data)) {
    ind <- which(names(Data) %in% Key)
    Insert <- paste0(names(Data[, -ind]), collapse = ",")
    Source <- paste0("S.", names(Data[, -ind]), collapse = ",")
    Updating <- paste0(lapply(names(Data[, -ind]), function(x) paste0("T.", x, " = S.", x)), collapse = ",")
  } else {
    Insert <- paste0(names(Data), collapse = ",")
    Source <- paste0("S.", names(Data), collapse = ",")
    Updating <- paste0(lapply(names(Data), function(x) paste0("T.", x, " = S.", x)), collapse = ",")
  }
  Matching <- paste0(lapply(By, function(x) paste0("T.", x, " = S.", x)), collapse = " AND ")

  ID <- paste0("##LOAD", round(runif(1, min = 1000000 , max = 9000000)))
  DBI::dbWriteTable(channel, ID, Data)

  glue::glue(
    "MERGE {TableName} AS T
    USING tempdb.[##LOADtmp] AS S
    ON ({Matching})
    WHEN NOT MATCHED
    THEN INSERT({Insert}) VALUES({Source})
    WHEN MATCHED
    THEN UPDATE SET {Updating}
    OUTPUT $action;"
  )

  DBI::dbRemoveTable(ODM, ID, catalog_name = "tempdb")
}
