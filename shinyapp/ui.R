library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

title <- "Germany's Next Top Risk Model - Dota2"

dashboardPage(skin="blue",

	dashboardHeader(title = title, titleWidth=400),
	dashboardSidebar(
	  sidebarMenu(
	    id = "tab",
			# insert menu items here; IconName from fontawesome website; tabName = identifier of tabItem below
      menuItem("Data Overview", tabName = "analysis1", icon = icon("line-chart")),
      menuItem("Plots", tabName = "analysis2", icon = icon("table")),
      menuItem("Gold Prediction", tabName = "analysis3", icon = icon("bar-chart"))
    )
	),
	dashboardBody(
		# add style 
		tags$head(tags$title("blaa")), # title
		tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
		tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),

# tab items; create one tab item per menuItem
tabItems( 
# id = "tab",
# ---------------------- Analyse Words in Set of filtered papers -----------------------
tabItem(tabName="analysis1",
  box(title="Analysis", status="primary", solidHeader=T, width="100%",
    fluidRow( 
      column( 2, sliderInput( "ui.filter.threshold", "Top % of most popular words to rank", 
                              min = 0, max = 100, value = 5 ) ),
      column( 7, plotOutput("server.wordcloud", 
                            width = "100%", height = "800px") ),
      column( 3, tableOutput("server.word.table") )
    )
	) # end of box
),

# -------------------- More Word data ----------------------------------------------
tabItem(tabName="analysis2",
  box(title="Analysis", status="primary", solidHeader=T, width="100%",
    fluidRow(
      column( 5, 
        strong("This section uses the keywords created in section \"Trend Analysis\" and computes more corresponding features.") )
      ),
    fluidRow(
      column( 2, selectizeInput("ui.stats.x.axis", "x-axis", choices = NULL ),
              selectizeInput("ui.stats.y.axis", "y-axis", choices = NULL ),
              selectizeInput("ui.stats.scale", "scale", 
                             choices=c("linear","logarithmic","square root"), selected="linear" ),
              actionButton( "ui.button.generatetable", "Generate complete word table and graph" ) ),
      column( 4, 
              fluidRow( column(4, selectizeInput( "complete.table.order", "Column by which to order the table",
                                                  choices = c("word", "count_ref", "count_new",
                                                              "ratio", "citation_count_avg", "SJR_avg",
                                                              "IPP_avg", "affiliation_count"),
                                                  selected = "ratio" ) ),
                        column(6, numericInput( "complete.table.n", "Amount of rows to show",
                                                min = 0, value = 25 ) )
                        ),
              tableOutput( "server.word.table.complete") ),
      column( 6, plotlyOutput( "server.word.stats.plot", height="800px" ) )
    )
  ) # end of box
), # end of tabItem

# ----------------------- More Paper Data -------------------------------------------
tabItem(
  tabName="analysis3",
  box(title="Analysis of the filtered paper set", status="primary", solidHeader=T, width="100%",
      fluidRow(
        column( 3, selectizeInput("ui.word.graphs.config", "Relative or absolute Graphs:", 
                                        choices = c("Relative" = TRUE, "Absolute" = FALSE), 
                                        select = FALSE),
                      br(),
                      strong("Most cited Papers:"), tableOutput("word.papers.mostcited"),
                      strong("Most relevant Affiliations:"), tableOutput("word.affiliation"),
                      strong("Most relevant Journals:"), tableOutput("word.journal") ),
              column( 9,
                      fluidRow( column(6, plotlyOutput("server.word.categories.plot", height = "800px") ),
                                column(6, plotlyOutput("server.word.byyear", height = "800px") ) )
              )
            )
        ) # end of box
) # end of tabitem


#--- End
) # end of tabItems
) # end of dashbordBody
) # end of dasbordPage
