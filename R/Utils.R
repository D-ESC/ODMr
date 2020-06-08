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
    Updating <- paste0(
      lapply(names(Data[, -ind]),
             function(x) paste0("T.", x, " = S.", x)), collapse = ",")
  } else {
    Insert <- paste0(names(Data), collapse = ",")
    Source <- paste0("S.", names(Data), collapse = ",")
    Updating <- paste0(
      lapply(names(Data),
             function(x) paste0("T.", x, " = S.", x)), collapse = ",")
  }
  Matching <- paste0(
    lapply(By,
           function(x) paste0("T.", x, " = S.", x)), collapse = " AND ")

  ID <- paste0("##LOAD", round(stats::runif(1, min = 1000000, max = 9000000)))

  DBI::dbWriteTable(channel, ID, Data)

  SQL <- glue::glue(
    "MERGE {TableName} AS T
    USING tempdb.[{ID}] AS S
    ON ({Matching})
    WHEN NOT MATCHED
    THEN INSERT({Insert}) VALUES({Source})
    WHEN MATCHED
    THEN UPDATE SET {Updating};"
  )
  success <- DBI::dbExecute(channel,
    SQL)

  DBI::dbRemoveTable(channel, ID, catalog_name = "tempdb")
  return(success)
}

get_values <- function(data, selected, active) {
  data$ODMdata %>%
    dplyr::semi_join(data$meta[active, ]) %>%
    dplyr::filter(index %in% selected())
}

upsert <- function(data, insert) {
  data$ODMdata <- data$ODMdata %>%
    dplyr::anti_join(insert %>%
                       dplyr::mutate(edited = TRUE),
                     by = "index") %>%
    dplyr::bind_rows(insert)
}

is_valid_odm <- function(Data, channel = ODM) {

  Sites <- odm_read_tbl(odm_tbl = "Sites", channel = channel)
  Variables <- odm_read_tbl(odm_tbl = "Variables", channel = channel)
  Methods <- odm_read_tbl(odm_tbl = "Methods", channel = channel)
  Sources <- odm_read_tbl(odm_tbl = "Sources", channel = channel)
  QCLevels <- odm_read_tbl(odm_tbl = "QualityControlLevels", channel = channel)

  if (!is.data.frame(Data))
    stop("Needs to be a data frame.")
  if (nrow(Data) < 1)
    stop("No data.")
  if (!("DataValue" %in% colnames(Data)))
    stop("Data values need to be in a column named 'DataValue'.")
  if (!is.numeric(Data$DataValue))
    stop("DataValue should be numeric")
  if (!("LocalDateTime" %in% colnames(Data)))
    stop("Date and time need to be in a column named 'LocalDateTime'.")
  Data$LocalDateTime <- try(as.POSIXct(Data$LocalDateTime))
  if (!lubridate::is.POSIXt(Data$LocalDateTime))
    stop("LocalDateTime should be class POSIXlt or POSIXct.")
  if (anyNA(Data$LocalDateTime))
    stop("LocalDateTime should not contain any NA values.")
  if (!("SiteID" %in% colnames(Data)))
    stop("A site ID referenced from the 'Sites' table should
      be in a column named 'SiteID'.")
  if (length(unique(Data$SiteID)) != 1)
    stop("Only one site can be evaluated at a time.")
  if (any(as.numeric(Data$SiteID) %% 1 > 0)) {
    stop("SiteID should be an integer value.")
  } else {
    Data$SiteID <- as.integer(Data$SiteID)
  }
  if (length(setdiff(Data$SiteID, Sites$SiteID) > 0))
    stop("SiteID does not exist in Sites table.")
  if (!("VariableID" %in% colnames(Data)))
    stop("A variable ID referenced from the 'Variables' table should
      be in a column named 'VariableID'.")
  if (length(unique(Data$VariableID)) != 1)
    stop("Only one variable can be evaluated at a time.")
  if (any(as.numeric(Data$VariableID) %% 1 > 0)) {
    stop("VariableID should be an integer value.")
  } else {
    Data$VariableID <- as.integer(Data$VariableID)
  }
  if (length(setdiff(Data$VariableID, Variables$VariableID) > 0))
    stop("VariableID does not exist in 'Variables' table.")
  if (!("MethodID" %in% colnames(Data)))
    stop("A method ID referenced from the 'Methods' table should
      be in a column named 'MethodID'.")
  if (length(unique(Data$MethodID)) != 1)
    stop("Only one method can be evaluated at a time.")
  if (any(as.numeric(Data$MethodID) %% 1 > 0)) {
    stop("MethodID should be an integer value.")
  } else {
    Data$MethodID <- as.integer(Data$MethodID)
  }
  if (length(setdiff(Data$MethodID, Methods$MethodID) > 0))
    stop("MethodID does not exist in 'Methods' table.")
  if (!("QualityControlLevelID" %in% colnames(Data)))
    stop("A quality control level ID referenced from the 'QualityControlLevels'
      table should be in a column named 'QualityControlLevelID'.")
  if (length(unique(Data$QualityControlLevelID)) != 1)
    stop("Only one quality control level can be evaluated at a time.")
  if (any(as.numeric(Data$QualityControlLevelID) %% 1 > 0)) {
    stop("QualityControlLevelID should be an integer value.")
  } else {
    Data$QualityControlLevelID <- as.integer(Data$QualityControlLevelID)
  }
  if (length(setdiff(Data$QualityControlLevelID,
                     QCLevels$QualityControlLevelID) > 0))
    stop("QualityControlLevelID does not exist in the
      'QualityControlLevels' table.")
  if (!("SourceID" %in% colnames(Data)))
    stop("A source ID referenced from the 'Sources' table should be in a
      column named 'SourceID'.")
  if (length(unique(Data$SourceID)) != 1)
    stop("Only data from one source can be evaluated at a time.")
  if (any(as.numeric(Data$SourceID) %% 1 > 0)) {
    stop("SourceID should be an integer value.")
  } else {
    Data$SourceID <- as.integer(Data$SourceID)
  }
  if (length(setdiff(Data$SourceID, Sources$SourceID) > 0))
    stop("SourceID does not exist in 'Sources' table.")
  if (!("UTCOffset" %in% colnames(Data)))
    stop("A UTC offset needs to be defined in a column named 'UTCOffset'.")
  if (Data$UTCOffset[1] < -12 | Data$UTCOffset[1] > 12)
    stop("Invalid UTCOffset. Value should be between -12 and 12.")
  if (anyDuplicated(Data %>%
                    dplyr::select(LocalDateTime, VariableID,
                                  MethodID, QualityControlLevelID)))
    stop("No duplicate data entries allowed.")
  TRUE
}

odm_summary <- function(Data, channel = ODM) {
  DataSeries <- Data %>%
    dplyr::select(UTCOffset, SiteID, VariableID, MethodID, SourceID,
                  QualityControlLevelID) %>%
    dplyr::distinct()
  N <-  names(odm_read_tbl(odm_tbl = "Seriescatalog", channel = channel))
  DataSeries <- DataSeries %>%
    dplyr::left_join(
      odm_read_tbl(odm_tbl = "Sites", ID = .$SiteID, channel = channel),
      by = "SiteID") %>%
    dplyr::left_join(
      odm_read_tbl(odm_tbl = "Variables", ID = .$VariableID, channel = channel),
      by = "VariableID") %>%
    dplyr::left_join(
      odm_read_tbl(odm_tbl = "Methods", ID = .$MethodID, channel = channel),
      by = "MethodID") %>%
    dplyr::left_join(
      odm_read_tbl(odm_tbl = "Sources", ID = .$SourceID, channel = channel),
      by = "SourceID") %>%
    dplyr::left_join(
      odm_read_tbl(odm_tbl = "QualityControlLevels",
                   ID = .$QualityControlLevelID, channel = channel),
      by = "QualityControlLevelID") %>%
    dplyr::mutate(
      VariableUnitsName =
        odm_read_tbl(odm_tbl = "Units",
                     ID = .$VariableUnitsID, channel = channel)$UnitsName) %>%
    dplyr::mutate(
      TimeUnitsName =
        odm_read_tbl(odm_tbl = "Units",
                     ID = .$TimeUnitsID, channel = channel)$UnitsName) %>%
    dplyr::mutate(BeginDateTime = min(Data$LocalDateTime)) %>%
    dplyr::mutate(EndDateTime = max(Data$LocalDateTime)) %>%
    dplyr::mutate(BeginDateTimeUTC = BeginDateTime -
                    (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
    dplyr::mutate(EndDateTimeUTC = EndDateTime -
                    (60 * 60 * (as.numeric(.$UTCOffset)))) %>%
    dplyr::mutate(ValueCount = length(Data$DataValue)) %>%
    .[, N[which(N %in% colnames(.))]]
  DataSeries
}
