library(shinydashboard)
library(shiny)

shinyUI(
  dashboardPage(

    dashboardHeader(title = "Bitpython"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
          badgeLabel = "new", badgeColor = "green")
        )
      ),

    dashboardBody(
      # add horvath style 
      tags$head(tags$title("Horvath and Partners")), # title
      tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
      tags$head(tags$style("#out_prev_status{color: red;}")),

      tabItems(
       tabItem(tabName = "dashboard",
         fluidPage(
          

           sidebarLayout(
             sidebarPanel(
              sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30)
              ),

            # Show a plot of the generated distribution
             mainPanel(
               box(title="Home", status="primary", solidHeader=T, width = 10,
                plotOutput("distPlot")
                )
               )
             )
           )
         ),

       tabItem(tabName = "widgets",
        fluidPage()
        # h2("Widgets tab content")
        )
       )
      )
    )
  )
