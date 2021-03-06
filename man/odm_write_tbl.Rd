% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odm_write_tbl.R
\name{odm_write_tbl}
\alias{odm_write_tbl}
\title{ODM write function}
\usage{
odm_write_tbl(x, odm_tbl = "Seriescatalog", channel = ODM)
}
\arguments{
\item{x}{dtaframe to load to ODM table}

\item{odm_tbl}{name of the table in the ODM database}

\item{channel}{connection handle as returned by odbcConnect
(default value = ODM)}
}
\value{
number of rows effected
}
\description{
This function can write meta data to an ODM database table
}
\examples{

\dontrun{
# Establish connection with database
ODM <- odbc::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD",
 UID = "update", PWD = rstudioapi::askForPassword("Database password"),
 Port = 1433)

# Get a list of sites in the database
Site <- odm_read_tbl(odm_tbl = "Sites")

# Get a list of data series in the database
odm_write_tbl(Site, odm_tbl = "Sites")
}

}
