sqlmerge <- function(Data, TableName, By, Key) {
  Data[is.na(Data)] <- "NULL"
  if (Key %in% names(Data)) {
    ind <- which(names(Data) %in% Key)
    Data <- Data[,-ind]
  }
  Values <- paste0(
    "(",
    apply(X = Data, FUN = function (x)
    paste0('\'', x, '\'', collapse = ','), MARGIN = 1),
    ")",
    collapse = ',')
  Values <- gsub("'NULL'", "NULL", Values)
  Names <- paste0(names(Data), collapse = ',')
  Source <- paste0('S.', names(Data), collapse = ',')
  Matching <- paste0(lapply(By, function(x) paste0('T.', x, ' = S.', x)), collapse = ' AND ')
  Updating <- paste0(lapply(names(Data), function(x) paste0('T.', x, ' = S.', x)), collapse = ',')
  glue::glue(
    "MERGE {TableName} AS T
    USING (VALUES {Values}) AS S({Names})
    ON ({Matching})
    WHEN NOT MATCHED
    THEN INSERT({Names}) VALUES({Source})
    WHEN MATCHED
    THEN UPDATE SET {Updating}
    OUTPUT $action, inserted.{Key};")}
