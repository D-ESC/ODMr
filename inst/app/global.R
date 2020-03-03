source("Seriescatalog_Module.R")
source("Plot_Module.R")
source("Stats_Module.R")
source("Console_Module.R")
source("Toolbox_Module.R")
source("Table_Module.R")
source("Import_Module.R")

library(magrittr)
library(rlang)
library(shiny)
library(ODMr)
options(shiny.maxRequestSize = 50 * 1024^2)

ODM <- pool::dbPool(odbc::odbc(), dsn = "ODM", database = "ODM",
                      UID = "update", PWD = "update", Port = 53493)
