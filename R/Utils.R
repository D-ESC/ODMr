sqlmerge <- function(Data, TableName, By) {
  Data[is.na(Data)] <- "NULL"
  Values <- paste0(apply(X = Data, FUN = function (x)
    paste0('\'', x, '\'', collapse = ','), MARGIN = 1), collapse = ',')
  Names <- paste0(names(Data), collapse = ',')
  Source <- paste0('S.', names(Data), collapse = ',')
  Matching <- paste0(lapply(By, function(x) paste0('T.', x, ' = S.', x)), collapse = ' AND ')
  Updating <- paste0(lapply(By, function(x) paste0('T.', x, ' = S.', x)), collapse = ',')
  glue::glue(
    "MERGE {TableName} AS T
    USING (VALUES({Values})) AS S({Names})
    ON ({Matching})
    WHEN NOT MATCHED
    THEN INSERT({Names}) VALUES({Source})
    WHEN MATCHED
    THEN UPDATE SET {Updating}
    OUTPUT $action, inserted.*;")}
