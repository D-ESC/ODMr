#' ODMr: A package for working with the CUAHSI ODM
#'
#' The aim of this package is to provide some functions for retrieving and
#' loading data into an instance of a MS-SQL CUAHSI ODM database. In order to
#' make use of these functions a valid ODBC connection for the target database
#' needs to be defined.
#'
#' @section ODMr functions: ODMselect can be used to retrieve data from an ODM
#'   database. ODMSelect returns an xts object containing the required columns
#'   for working with ODM data.
#'
#'   ODMload can be used to get data into an ODM database. ODMload takes an xts
#'   object containing the required columns for working with ODM data and loads
#'   it. The xts object will usually have been acquired using ODMselect.
#'
#' @docType package
#' @name ODMr
NULL
