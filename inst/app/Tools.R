source('~/GitHub/ODMtools/SeriesCatalog_Module.R')
source('~/GitHub/ODMtools/Plot_Module.R')
source('~/GitHub/ODMtools/Stats_Module.R')
source('~/GitHub/ODMtools/Console_Module.R')
source('~/GitHub/ODMtools/Toolbox_Module.R')
source('~/GitHub/ODMtools/Table_Module.R')

library(magrittr)

getValues <- function(data, selected) {
  data$ODMdata %>% dplyr::semi_join(data$meta[1,]) %>% 
    dplyr::filter(index %in% selected(), QualityControlLevelID > 0) %>%
    dplyr::mutate(edited = TRUE)
}

upsert <- function(data, insert) {
  data$ODMdata <- data$ODMdata %>% dplyr::anti_join(insert, by = "index") %>% 
    dplyr::bind_rows(insert)
}

ODM <- DBI::dbConnect(odbc::odbc(), dsn = "ODM", database = "ODM", 
                      UID = "update", PWD = "update", Port = 1433)

ODMtools <- function() {
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "ODM TOOLS"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Catalog", tabName = "catalog", icon = shiny::icon("list")),
        shinydashboard::menuItem("Plot", tabName = "plot", icon = shiny::icon("bar-chart-o")),
        shinydashboard::menuItem("Table", tabName = "datatable", icon = shiny::icon("table")),
        shinydashboard::menuItem("Stats", tabName = "stat", icon = shiny::icon("calculator")),
        shinydashboard::menuItem("Toolbox", tabName = "toolbox", icon = shiny::icon("wrench"))
      ),
      shiny::br(),
      shiny::br(),
      shiny::tags$head(
        shiny::tags$style(
          ".butt{margin-left: 15px;}"
        )
      ),
      shiny::downloadButton("downloadData.csv", "Download", class = "butt")
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "catalog",
          SeriesCatalog_ui("getData")
        ),
        shinydashboard::tabItem(
          tabName = "plot",
          Plot_ui("getPlot")
        ),
        shinydashboard::tabItem(
          tabName = "datatable",
          Table_ui("getTable")
        ),
        shinydashboard::tabItem(
          tabName = "stat",
          Stat_ui("getStat")
        ),
        shinydashboard::tabItem(
          tabName = "toolbox",
          shiny::h4("editing:"),
          shiny::tableOutput("activeseries"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(width = 6,
                   Toolbox_ui("getToolbox")),
            shiny::column(width = 6,
                   Console_ui("getConsole"))
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    ODMdata <- shiny::callModule(SeriesCatalog_server, "getData", connection = ODM)
    brushed <- shiny::callModule(Plot_server, "getPlot", data = ODMdata)
    shiny::callModule(Table_server, "getTable", data = ODMdata, selected = brushed)
    shiny::callModule(Stat_server, "getStat", data = ODMdata, selected = brushed)
    shiny::callModule(Toolbox_server, "getToolbox", connection = ODM, data = ODMdata, selected = brushed)
    shiny::callModule(Console_server, "getConsole", data = ODMdata, selected = brushed)
    output$activeseries <- shiny::renderTable(ODMdata$meta[1,])
    output$downloadData.csv <- shiny::downloadHandler(
      filename = function() {
        paste("ODM_", gsub(":", "-", Sys.time()), ".csv", sep = "")
      },
      content = function(file) {
        utils::write.csv(ODMdata$ODMdata %>% dplyr::select(-index),
                         file,
                         row.names = FALSE
        )
      }
    )
  }
  
  shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
}