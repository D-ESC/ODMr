ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "ODM TOOLS"),
  shinydashboard::dashboardSidebar(
    shinyjs::useShinyjs(debug = TRUE),
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Catalog", tabName = "catalog", icon = shiny::icon("list")),
      shinydashboard::menuItem("Plot", tabName = "plot", icon = shiny::icon("bar-chart-o")),
      shinydashboard::menuItem("Table", tabName = "datatable", icon = shiny::icon("table")),
      shinydashboard::menuItem("Stats", tabName = "stat", icon = shiny::icon("calculator")),
      shinydashboard::menuItem("Toolbox", tabName = "toolbox", icon = shiny::icon("wrench")),
      shinydashboard::menuItem("Model", tabName = "model", icon = shiny::icon("line-chart")),
      shinydashboard::menuItem("Import", tabName = "import", icon = shiny::icon("upload"))
    ),
    shiny::br(),
    shiny::br(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("#downloadData{margin-left: 15px;color: #333;}"))
    ),
    shiny::downloadButton("downloadData", "Download", class = "butt")
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
        DT::dataTableOutput("activeseries1"),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(width = 6,
                        Toolbox_ui("getToolbox")),
          shiny::column(width = 6,
                        Console_ui("getConsole"))
        )
      ),
      shinydashboard::tabItem(
        tabName = "model",
        DT::dataTableOutput("activeseries2"),
        shiny::br(),
        Model_ui("getModel")
        ),
      shinydashboard::tabItem(
        tabName = "import",
        Import_ui("getImport")
      )
    )
  )
)