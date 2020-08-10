

shinyUI(dashboardPage(
  dashboardHeader(title = "Chipotle Locations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("About Me", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "map",
              tabsetPanel(type = "tabs",id = "chart_tabs",
              tabPanel("State Count", 
                       
              fluidRow(box(htmlOutput("map"), height = "auto", width="auto"))))),
              
                       
      tabItem(tabName = "plots",
              tabsetPanel(type = "tabs",id = "chart_tabs",
              tabPanel("Box Plots",
                        fluidRow(box(plotOutput("county"), height = "auto", width="auto" )),
                       radioButtons(
                         inputId="feature_select",
                         label="Select Feature",
                         choices=list("per_capita_income", "pop_per_store", "Population_Density")
                       ),
                       selectizeInput(
                         inputId="state_select",
                         label="Select State",
                         choices=unique(sort(chipotles$state))
                       )),
              tabPanel("Scatter Plot",
                       fluidRow(box(plotOutput("scatter"), height = "auto", width = "auto")),
                       selectizeInput(
                         inputId="feature_select2",
                         label="Select Feature",
                         choices=list("Total_Population","per_capita_income", "white", "black", "asian", "american_indian", "native_hawaiian_pacific_islander", "two_or_more_races", "hispanic_or_latino")
                       ))
              )),
      tabItem(tabName = "data",
              tabsetPanel(type = "tabs",id = "chart_tabs",
                          tabPanel("County Demographics",          
                          fluidRow(box(DT::dataTableOutput("table"), width = 12))),
                          tabPanel("Store Locations",
                          fluidRow(box(DT::dataTableOutput("table2"), width = 12)))
                          )),
              
      
      
      tabItem(tabName = "about",
              fluidRow("Hi I'm Jack"))
    )
    )
  )
)