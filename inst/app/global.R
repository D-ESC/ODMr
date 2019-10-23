source('SeriesCatalog_Module.R')
source('Plot_Module.R')
source('Stats_Module.R')
source('Console_Module.R')
source('Toolbox_Module.R')
source('Table_Module.R')
source('Import_Module.R')
source('Model_Module.R')

library(magrittr)
library(rlang)
library(shiny)
options(shiny.maxRequestSize=50*1024^2)

getValues <- function(data, selected, active) {
  data$ODMdata %>% dplyr::semi_join(data$meta[active,]) %>%
    dplyr::filter(index %in% selected())
}

upsert <- function(data, insert) {
  data$ODMdata <- data$ODMdata %>% dplyr::anti_join(insert, by = "index") %>%
    dplyr::bind_rows(insert)
}

ODM <- pool::dbPool(odbc::odbc(), dsn = "ODM", database = "ODM",
                      UID = "update", PWD = "update", Port = 53493)
