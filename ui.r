library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)

# runApp(host = "0.0.0.0", port=5050)


shinyUI(bootstrapPage(theme = "bootstrap.css",
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "icon", type = "text/css", href = "tclrslogo.png")
    
  ),
  
  tags$head(
    tags$link(rel = "manifest", href = "manifest2.json")
    
    
  ),
  
  tags$head(tags$script(src="app.js")),
  
  tags$head(tags$script(src="sw.js")),
  
  includeCSS("mystyle.css"),
  
  dashboardPage(title="TCLRS", skin = "blue",
                dashboardHeader(title = tags$div(class = "mine",align ="left",img(src="tclrslogo.png"),"TCLRS")
                                                                # 
                                # tags$li(class="dropdown",
                                #         tags$style(".main-header {max-height: 20px}"),
                                #         tags$style(".main-header .logo {height: 20px;}"),
                                #         tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
                                #         tags$style(".navbar {min-height:20px !important}")
                                # )
                ),
                dashboardSidebar(  
                  
                  sidebarMenu(id="tabs", class="pull-top",
                    # h5("Preliminaries"),
                    menuItem("Set Date and Time", tabName = "sd"),
                    menuItem("Set location", tabName = "sl"),
                    hidden(div( id="haha", class="sidebar-menu",
                                    h5(tags$em("Statistical Recomendations")),
                    menuItem("Can't miss a customer" , tabName = "lrc"),
                    menuItem("Most probably find a customer", tabName = "lrp"),
                    menuItem("Recommendation closest to you", tabName = "lr"),
                    menuItem(tags$em("Other Statistics"),
                    menuItem("All Time General", tabName = "atg"),
                    menuItem("Yesterday", tabName = "yd"),
                    menuItem("Last 7 days", tabName = "l7d"),
                    menuItem("Last 1 Month", tabName = "l1m"),
                    menuItem("Last week", tabName = "lw"),
                    menuItem("Last month", tabName = "lm"),
                    menuItem("Every past week", tabName = "epw"))
                    ))
                  )),
                dashboardBody(
                  tabItems(	
                    tabItem(tabName = "sl",
                            withSpinner(uiOutput("mapi"))),
                    
                    tabItem(tabName = "sd",
                            withSpinner(uiOutput("msdi"))),
                    
                    tabItem(tabName = "lr",
                            withSpinner(uiOutput("mlrdi"))),
                    
                    tabItem(tabName = "lrp",
                            withSpinner(uiOutput("mlrpi"))),
                    
                    tabItem(tabName = "lrc",
                            withSpinner(uiOutput("mlrci"))),
                    
                    
                    tabItem(tabName = "atg",
                            withSpinner(uiOutput("matgi"))),

                    tabItem(tabName = "yd",
                            withSpinner(uiOutput("mydi"))),

                      tabItem(tabName = "l7d",
                              withSpinner( uiOutput("ml7di")
                      )),
                     tabItem(tabName = "l1m",
                             withSpinner(uiOutput("ml1mi"))
                     ),
                     tabItem(tabName = "lw",
                             withSpinner(uiOutput("mlwi"))
                    ),
                     tabItem(tabName = "lm",
                             withSpinner(uiOutput("mlmi"))
                     ),
                     tabItem(tabName = "epw",
                             withSpinner(uiOutput("mepwi"))
                     )
                  )
                )
  )
)
)
              