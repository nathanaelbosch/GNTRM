library(shiny)
library(ggplot2)
library(plotly)

# color definitions
hpdarkblue  <- rgb(5,65,90,max=255)
hplightblue <- rgb(0,140,200, max=255)

path_scripts <- "~/Projekte/gntrm/scripts/"

source(paste0(path_scripts, "combination.R"))
# source("~/Projekte/arXiv_app2/arxiv_app_data_initialize.R")
# source("~/Projekte/arXiv_app2/arxiv_app_functions.R")

####################################
### Shiny server
####################################
shinyServer(function(input, output, session) {
  
# rV <- reactiveValues( papers = papers, papers_filtered = papers, words = NULL,
#                       word_table = NULL, filter.word = NULL, 
#                       timeframe = c("2005-1-1" %>% as.Date, "2014-1-1" %>% as.Date, 
#                                     Sys.Date() %>% as.Date), 
#                       threshold = 5)

# #-------------------- Filter Papers Box ---------------------
# # initialize the category and journal filters
# updateSelectizeInput( session, "ui.filter.category",
#                       choices = categories_all$category, server=T )
# updateSelectizeInput( session, "ui.filter.journal",
#                       choices = journals_all, server=T )

# # control values: amount of papers, amount of words
# output$server.papers.n <- renderText( rV$papers %>% nrow )
# output$server.papers.loaded.ui <- renderUI({
#   list( strong("# papers total:"), textOutput("server.papers.n") )
# })
# output$server.papers.filtered.n <- renderText( rV$papers_filtered %>% nrow )
# output$server.papers.filtered.n.ui <- renderUI({
#   if( !is.null(rV$papers_filtered) & !(nrow(rV$papers) == nrow(rV$papers_filtered)) )
#     list( strong("# papers after filtering:"), textOutput("server.papers.filtered.n") )
# })
# output$server.papers.filtered.words.n <- renderText( rV$words %>% nrow )
# output$server.papers.filtered.words.n.ui <- renderUI({
#   if( !is.null(rV$words) )
#     list( strong("# unique keywords in those papers:"), textOutput("server.papers.filtered.words.n") )
# })
# output$server.words.n <- renderText( rV$words %>% 
#                                        format_wordtable(input$ui.filter.threshold) %>% nrow )
# output$server.words.n.ui <- renderUI({
#   if( !is.null(rV$words) )
#     list( strong("# keywords used for ranking:"), textOutput("server.words.n") )
# })



# # Button to apply the filters - With progressbar comments (we leave them out in following cases)
# observeEvent( input$ui.button.filter.papers, {
#   # Create a Progress object
#   progress <- shiny::Progress$new()
#   progress$set(message = "Computing data", value = 0)
#   # Close the progress when this reactive exits (even if there's an error)
#   on.exit(progress$close())
#   updateProgress <- function(value = NULL, detail = NULL) {
#     if (is.null(value)) {
#       value <- progress$getValue()
#       value <- value + (progress$getMax() - value) / 5
#     }
#     progress$set(value = value, detail = detail)
#   }
#   updateProgress(detail = "Processing word")
#   rV$filter.word <- input$ui.filter.word %>% gsub(pattern = "^ +", replacement = "", x = .) %>% 
#     gsub(pattern = " +$", replacement = "", x = .) %>% tolower
#   updateProgress(detail = "Filtering papers")
#   rV$papers_filtered <- filter_papers_by( papers, rV$filter.word, input$ui.filter.category, 
#                                           input$ui.filter.journal, input$ui.filter.SJR, input$ui.filter.IPP )
# })

# #-------------------- Generate Words Box ---------------------
# output$word.box <- renderUI({
#   if( input$tab %in% c("analysis1", "analysis2") ){
#   box(title = "Generate Words", status="primary", solidHeader=T, width="100%",
#       uiOutput( "slider.new" ),
#       uiOutput( "slider.ref" ),
#       actionButton( "ui.button.words", "Generate words and rank according to timeframe" ) )
#   }
# })

# #-------------------- Analysis 1 Tab --------------------
# # Generate sliders as uiOutput to adjust them better
# output$slider.new <- renderUI({ sliderInput("ui.filter.timeframe.new", "Trend timeframe", 
#                                             "2000-1-1" %>% as.Date, Sys.Date() %>% as.Date, 
#                                             c(rV$timeframe[2], rV$timeframe[3])) })
# output$slider.ref <- renderUI({ sliderInput("ui.filter.timeframe.ref", "Reference timeframe", 
#                                             "2000-1-1" %>% as.Date, Sys.Date() %>% as.Date, 
#                                             c(rV$timeframe[1], rV$timeframe[2] )) })
# observeEvent( input$ui.filter.timeframe.ref, {
#   rV$timeframe[1:2] = input$ui.filter.timeframe.ref
# })
# observeEvent( input$ui.filter.timeframe.new, {
#   rV$timeframe[2:3] = input$ui.filter.timeframe.new
# })

# # listen to threshold slider, save value and update slider
# observeEvent( input$ui.filter.threshold, {
#   rV$threshold <- input$ui.filter.threshold
#   updateSliderInput( session, "ui.filter.threshold", 
#                      min = 0, max = min(ceiling(max(2*rV$threshold,1)), 100),
#                      step = 0.1 )
# })

# output$server.words.n <- renderText( rV$words %>% format_wordtable(input$ui.filter.threshold) %>% 
#                                        nrow )

# # word-table output
# output$server.word.table <- renderTable( rV$words %>% format_wordtable(input$ui.filter.threshold) %>% 
#                                            head(20) )
# # word-cloud output
# output$server.wordcloud <- renderPlot( rV$words %>% format_wordtable(input$ui.filter.threshold) %>% 
#                                          get_wordcloud(n = 100) )

# # Get words and rank them!
# observeEvent( input$ui.button.words, {
#   progress <- shiny::Progress$new()
#   progress$set(message = "Computing data", value = 0)
#   on.exit(progress$close())
#   updateProgress <- function(value = NULL, detail = NULL) {
#     if (is.null(value)) {
#       value <- progress$getValue()
#       value <- value + (progress$getMax() - value) / 5
#     }
#     progress$set(value = value, detail = detail)
#   }
#   updateProgress(detail = "Generating words")
#   rV$words <- get_words(rV$papers_filtered, rV$timeframe %>% as.POSIXct)
#   updateProgress(value = 1, detail = "Finished")
# })


# #-------------------- Analysis 2 Tab --------------------
# updateSelectizeInput( session, "ui.stats.x.axis",
#                       choices = c("count_new", "ratio", "citation_count_avg", 
#                                   "SJR_avg", "IPP_avg", "affiliation_count"), server=T )
# updateSelectizeInput( session, "ui.stats.y.axis",
#                       choices = c("count_new", "ratio", "citation_count_avg", 
#                                   "SJR_avg", "IPP_avg", "affiliation_count"), server=T )

# output$server.word.stats.plot <- renderPlotly({
#   if( all( c(input$ui.stats.x.axis, input$ui.stats.y.axis) != "") ){
#     if( is.null(rV$word_table) ) return(ggplotly(p <- ggplot()))
#     d <- rV$word_table
#     # normalize d? interesting for PCA, less interesting for simple visualisation
#     #       d[, count_new := scale(count_new)]
#     #       d[, ratio := scale(ratio)]
#     #       d[, citation_count_avg := scale(citation_count_avg)]
#     #       d[, SJR_avg := scale(SJR_avg)]
#     #       d[, IPP_avg := scale(IPP_avg)]
#     #       d[, affiliation_count := scale(affiliation_count)]
#     d$x <- d %>% getElement(input$ui.stats.x.axis)
#     d$y <- d %>% getElement(input$ui.stats.y.axis)
#     p <- ggplot( data=d, aes(x=x,y=y, word = word, count_new = count_new,
#                              ratio = ratio, citation_count_avg = citation_count_avg,
#                              SJR_avg = SJR_avg, IPP_avg = IPP_avg, 
#                              affiliation_count = affiliation_count) ) + 
#       geom_point(colour=hplightblue) + 
#       xlab( input$ui.stats.x.axis ) + ylab( input$ui.stats.y.axis )
#     if( input$ui.stats.scale == "logarithmic" ) p <- p + scale_x_log10() + scale_y_log10()
#     if( input$ui.stats.scale == "square root" ) p <- p + scale_x_sqrt() + scale_y_sqrt()
#     ggplotly(p)
#   }
#   else return(ggplotly(p <- ggplot()))
# })
# output$server.word.table.complete <- renderTable(
#   rV$word_table %>% arrange( input$complete.table.order ) %>% head( input$complete.table.n ) )

# observeEvent( input$ui.button.generatetable, {
#   {
#     progress <- shiny::Progress$new()
#     progress$set(message = "Generating table", value = 0)
#     on.exit(progress$close())
#     updateProgress <- function(value = NULL, detail = NULL) {
#     if (is.null(value)) {
#       value <- progress$getValue()
#       value <- value + (progress$getMax() - value) / 5
#     }
#     progress$set(value = value, detail = detail)
#     }
#   }
#   updateProgress(detail = "Button pressed")
#   updateProgress(detail = "")
#   rV$word_table <- rV$words %>%
#     format_wordtable(threshold = input$ui.filter.threshold) %>% 
#     head(50) %>% get_word_data_table( papers = rV$papers, parallel = F )
#   updateProgress(value = 1, detail = "Finished")
# })

# #-------------------- Analysis 3 Tab --------------------
# output$server.word.categories.plot <- renderPlotly( get_categories(rV$papers_filtered, plot = TRUE, 
#                                                                    rel = input$ui.word.graphs.config) )
# output$server.word.byyear <- renderPlotly({
#   if(is.null(rV$papers_filtered)) return(ggplotly())
#   year_count <- rV$papers_filtered %>% mutate(year = year(coverDate)) %>%
#     .[year >= 2000] %>% .[, .N, by=(year)]  %>% 
#     merge(data.table("year" = c(2000:2016)), by = "year", all = TRUE) %>% arrange(year)
#   year_count[is.na(year_count)] <- 0
#   year_count[, "R" := year_count$N / count_per_year[year %in% year_count$year]$N]
#   year_count$y = year_count %>% getElement(ifelse(input$ui.word.graphs.config, "R", "N"))
#   year_count$year = factor(year_count$year)
#   setnames(year_count, "N", "Count")
#   p <- ggplot( data = year_count, aes(x = year, y = y, fill = factor(y), count = Count)) + 
#     geom_bar(stat = "identity") + xlab("\n\n ") + ylab("") +
#     ggtitle(paste("Count per year", ifelse(input$ui.word.graphs.config, "(relative)", "(absolute)"))) +
#     scale_fill_manual(name = "\n\n ", 
#                       values = hppalette(max(year_count$year %>% as.character %>% as.numeric()) - 
#                                            min(year_count$year %>% as.character %>% as.numeric())+1)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
#   ggplotly(p)
# })

# output$word.papers.mostcited = renderTable(
#   rV$papers_filtered %>% arrange(desc(citedby.count))  %>% 
#     .[, .(title, "citation count" = citedby.count)] %>% head(5) )
# output$word.affiliation = renderTable(
#   rV$papers_filtered[affilname != ""] %>% getElement("affilname") %>% table %>% 
#     data.table %>% filter(N > 0) %>% arrange(desc(N)) %>% 
#     setnames(c(".", "N"), c("affiliation", "count")) %>% head(5) )
# output$word.journal = renderTable(
#   rV$papers_filtered$journal_title %>% table %>% data.table %>% 
#     filter(N>0) %>% arrange(desc(N)) %>% setnames(c(".", "N"), c("journal", "count")) %>% head(5) )

})

