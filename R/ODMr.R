#' ODMr: A package for working with the CUAHSI ODM
#'
#' The aim of this package is to provide some functions for retrieving and
#' loading data into an instance of a MS-SQL CUAHSI ODM database. In order to
#' make use of these functions a valid ODBC connection for the target database
#' needs to be defined.
#'
#' @section ODMr functions: odm_read can be used to retrieve data from an ODM
#'   database. odm_read returns a dataframe containing the required columns
#'   for working with ODM data.
#'
#'   odm_write can be used to get data into an ODM database. ODMload takes an
#'   dataframe containing the required columns for working with ODM data and loads
#'   it. The dataframe will usually have been acquired using ODMselect or created
#'   using odm_data.
#'
#' @docType package
#' @name ODMr
NULL
