library(shiny)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(DT)

#if(interactive()) {
ui<- dashboardPage(skin = "red",
                   dashboardHeader(title = "SRS ControlBoard",tags$li(a(onclick = "openTab('tabItem33')",
                                                                        href = NULL,
                                                                        icon("home"),
                                                                        title = "Homepage",
                                                                        style = "cursor: pointer;"),
                                                                      class = "dropdown",
                                                                      tags$script(HTML("
                                       var openTab = function(tabItem33){
                                       $('a', $('.sidebar')).each(function() {
                                       if(this.getAttribute('data-value') == tabItem33) {
                                       this.click()
                                       };
                                       });
                                       }")))),
                   dashboardSidebar(width = 200,
                                    sidebarMenu(
                                      menuItem("Home", tabName = "tabItem33", selected = T),
                                      #menuItem("Computations", tabName = "tabItem1", icon = icon("dashboard")),
                                      menuItem("Scheduler", tabName = "tabItem2", icon = icon("th")),
                                      menuItem("Event", tabName = "tabItem3", icon = icon("th")),
                                      menuItem("Log", tabName = "tabItem4", icon = icon("th"))
                                      
                                    )
                   ), 
                   dashboardBody(
                     tags$style(".small-box.bg-yellow { background-color: white !important; color: blue !important; }"),
                     tags$style(".small-box{border-radius: 15px}"),
                     tags$style(".box{border-radius: 15px}"),
                     #tags$head(tags$style(HTML("a {color: black}"))),
                     tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
                     
                     frow1 <- fluidRow(
                       valueBoxOutput("scheduler"),
                       valueBoxOutput("event"),
                       valueBoxOutput("log")
                     ),
                     #----------------scheduler--------------------- 
                     tabItems(
                       tabItem(tabName = "tabItem33",
                               tabsetPanel(type="tab",
                                 tabPanel("scheduler",
                                          
                                          fluidRow(
                                            box(title = "Time Taken by Jobcard",width = 12, collapsible = TRUE, plotlyOutput("plot1_f", height = "300px")),
                                            box(title = "Time Taken by Retail", collapsible = TRUE, plotlyOutput("plot2_f" , height = "300px")),
                                            box(title = "Time Taken by UpdateCalls", collapsible = TRUE, plotlyOutput("plot3_f", height = "300px"))
                                          )),
                                 #-------------------API-------------------------
                                 tabPanel("Event",
                                          fluidRow(
                                            box(title = "Time Taken by login API",width = 12, collapsible = TRUE, plotlyOutput("plot4_f", height = "300px")),
                                            box(title = "Count API", collapsible = TRUE, plotlyOutput("plot5_f" , height = "300px")),
                                            box(title = "Error Status of API", collapsible = TRUE, plotlyOutput("plot6_f", height = "300px"))
                                          )
                                 ))),
                                 
                                 
                            
                                 # fluidPage(
                                 # tabItems(
                                 tabItem(tabName = "tabItem2",
                                         tabsetPanel(type="tab",
                                                     tabPanel("Scheduler",
                                                              fluidRow(
                                                              box(title = "Time Taken by Jobcard",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot1", height = "300px")),
                                                              box(title = "Time Taken by Retail",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot2" , height = "300px")),
                                                              box(title = "Time Taken by UpdateCalls",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot3", height = "300px" )),
                                                              box(title = "Time Taken by Booking",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot4", height = "300px")),
                                                              box(title = "Scheduler Error Status Count", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot5", height = "300px" )),
                                                              box(title = "No Of Scheduler runs",solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot6", height = "300px"))
                                                     )),
                                                     tabPanel("Scheduler Current Date",
                                                              fluidRow(
                                                              box(title = "Time Taken by Jobcard", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot7", height = "300px")),
                                                              box(title = "Time Taken by Retail", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot8", height = "300px")),
                                                              box(title = "Time Taken by Updatecall", Sys.Date(), status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot9", height = "300px")),
                                                              box(title = "Time Taken by Booking", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot10", height = "300px")),
                                                              box(title = "Success Failure Error Status count", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot11", height = "300px")),
                                                              box(title = "No Of Scheduler runs", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot12", height = "300px"))
                                                     )),
                                                     tabPanel("Dates and date ranges",
                                                              dateRangeInput('dateRange',
                                                                             label = 'Date range input: yyyy-mm-dd',
                                                                             start = Sys.Date() - 2, end = Sys.Date()),
                                                              
                                                              fluidRow(
                                                              box(title = "Time Taken by Jobcard",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot27", height = "300px")),
                                                              box(title = "Time Taken by Retail",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot29", height = "300px")),
                                                              box(title = "Time Taken by UpdateCalls",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot28", height = "300px")),
                                                              box(title = "Time Taken by Booking", solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot30", height = "300px")),
                                                              box(title = "Scheduler Error Status count", solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot31", height = "300px")),
                                                              box(title = "No Of Scheduler runs",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot32", height = "300px"))
                                                     ))
                                         )),
                                 tabItem(tabName = "tabItem3",
                                         tabsetPanel(type="tab",
                                                     tabPanel("API",
                                                              fluidRow(
                                                              box(title = "Time Taken by Login", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot15", height = "300px")),
                                                              box(title = "Time Taken by Logout",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot16", height = "300px")),
                                                              box(title = "API Error Status count",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot17", height = "300px")),
                                                              box(title = "No Of API Called",  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot18", height = "300px"))
                                                     )),
                                                     tabPanel("API Running status on Current Date",
                                                              fluidRow(
                                                              box(title = "Time taken by Login", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot19", height = "300px")),
                                                              box(title = "Time taken by Logout", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot20", height = "300px")),
                                                              box(title = "API Error Status count", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot21", height = "300px")),
                                                              box(title = "No of API Called", Sys.Date(),  solidHeader = TRUE, collapsible = TRUE, plotlyOutput("plot22", height = "300px"))
                                                     )),
                                                     tabPanel("API Running status between date ranges",
                                                              dateRangeInput('dateRangeA',
                                                                             label = 'Date range input: yyyy-mm-dd',
                                                                             start = Sys.Date() - 2, end = Sys.Date()),
                                                              fluidRow(
                                                              box(title = "Time taken by Login",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot33", height = "300px")),
                                                              box(title = "Time taken by Logout",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot34", height = "300px")),
                                                              box(title = "API Error Status count",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot35", height = "300px")),
                                                              box(title = "No of API called",  solidHeader = TRUE, collapsible = TRUE, align="center", plotlyOutput("plot36", height = "300px"))
                                                     ))
                                         )
                                 )
                     )
                   )
)
#}
shinyApp(ui,server)