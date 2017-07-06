# functions needed for the shiny-app

# filter a given set of papers by occurrence of a given word in the keywords
# two methods: check if the word equals a keywords, or check via grepl
# grep = FALSE is way faster than grep = TRUE
filter_by_word = function(papers = papers, word){
  if( is.null(papers) )
    return(NULL)
  if( is.null(word) || word == "" || word == " " || is.na(word) )
    return(papers)
  p <- papers[lapply(papers$keywords, function(x) {word %in% x}) %>% unlist]
  return( p )
}

filter_papers_by = function( papers, word = NULL, category = NULL, journal = NULL, 
                             SJR_min = NULL, IPP_min = NULL ){
  papers_out <- papers
  if( !(is.null(word) || word == "" || is.na(word)) )
    papers_out <- filter_by_word(papers_out, word)
  if( !(is.null(category) || category == "" || is.na(category)) ){
    g = list(NULL)
    g[[1]] = grepl(category[1], papers_out$categories %>% as.character)
    if( length(category)>2){
      for(i in 2:length(category)){
        g[[2]] = grepl(category[i], papers_out$categories %>% as.character)
        g[[1]] = g[[1]] | g[[2]]
      }
    }
    papers_out <- papers_out[g[[1]]]
  }
  if(!(is.null(journal) || journal == "" || is.na(journal)) ){
    journal <- journal %>% unlist
    papers_out <- papers_out[journal_title %in% journal]
  }
  if( !(is.null(SJR_min) || SJR_min == "" || SJR_min == 0) )
    papers_out <- papers_out[SJR > SJR_min]
  if( !(is.null(IPP_min) || IPP_min == "" || IPP_min == 0) )
    papers_out <- papers_out[IPP > IPP_min]
  return(papers_out)
}

# get the relevant words for a given set of papers - relevance by timeframe
get_words = function(papers, timeframe = c("2005-1-1" %>% as.POSIXct, 
                                           "2014-1-1" %>% as.POSIXct, 
                                           Sys.Date() %>% as.POSIXct)) {
  if(is.null(papers)) return(NULL)
  papers_reference = papers[(papers$coverDate > timeframe[1]) &
                              (papers$coverDate < timeframe[2])]
  papers_new = papers[(papers$coverDate > timeframe[2]) &
                        (papers$coverDate < timeframe[3])]
  word_table_ref = papers_reference$keywords %>% unlist %>% table %>% data.table
  word_table_new = papers_new$keywords %>% unlist %>% table %>% data.table
  names(word_table_ref) = c("word", "count_ref")
  names(word_table_new) = c("word", "count_new")
  # combine the resulting tables:
  word_table = merge(word_table_ref, word_table_new, by = "word", all.y = TRUE) %>% data.table
  word_table[is.na(count_ref)] <- 0
  # use a ratio to define our interest
  word_table[, "ratio" := (count_new / (((timeframe[3] %>% as.POSIXct()) - (timeframe[2] %>% as.POSIXct())
  ) %>% as.numeric(units = "days")) / (count_ref / (((timeframe[2] %>% as.POSIXct()) - (timeframe[1] %>% as.POSIXct())
  ) %>% as.numeric(units = "days"))))]
  return(word_table[order(-ratio)])
}

get_categories = function(papers, plot = TRUE, rel = TRUE) {
  if(is.null(papers)) return(NULL)
  cats = papers$categories %>% as.character %>% strsplit(split = " ") %>% unlist
  table = cats %>% table %>% data.table %>% .[order(.)]
  if(rel) table[, "R" := table$N/categories_all[category %in% table$.]$N]
  # cut after 10 most important categories
  table$y = table %>% getElement(ifelse(rel, "R", "N"))
  # table %<>% arrange(y)
  table %<>% arrange(desc(N))
  table %<>% head(10)
  setnames(table, c(".", "N"), c("Category", "Count"))
  g <- ggplot(data = table, aes(x = Category, y = y, fill = Category, count = Count)) + 
    geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("") + ylab("") +
    ggtitle(paste("Count per category", ifelse(rel, "(relative)", "(absolute)"))) +
    scale_fill_manual(name = "category", values = hppalette(nrow(table)))
  ifelse(plot, return(ggplotly(g)), return(table[,.(., P)]))
}

# find interesting values concerning a given word
get_word_data = function(word, papers = papers){
  papers_filtered = filter_by_word(papers = papers, word = word)
  ## frequency absolute
  # already availible through the word_table
  ## frequency relative
  # already availible through the word_table
  ## avg citation count
  citation_count_avg = mean(papers_filtered$citedby.count)
  ## avg journal rank SJR
  SJR_avg = mean(papers_filtered$SJR)
  ## avg journal rank IPP
  IPP_avg = mean(papers_filtered$IPP)
  ## amount of different affiliations
  affiliation_count = papers_filtered$affilname %>% uniqueN
  return(data.table(citation_count_avg, SJR_avg, IPP_avg, affiliation_count))
}

get_word_data_table = function(word_table, papers = papers, parallel = FALSE, cores = 18){
  # if( is.null(word_table) ) return(NULL)
  words = word_table$word
  
  if(parallel){
    cl = makeCluster(cores)
    clusterEvalQ(cl, {library(magrittr); library(dplyr); library(data.table)})
    clusterEvalQ(cl, {
      filter_by_word = function(papers, word){
        if(is.null(papers)) return(NULL)
        if(is.null(word)) return(papers)
        return(papers[sapply(papers$keywords, function(x) {word %in% unlist(x)})])
      }
    })
    clusterEvalQ(cl, {
      get_word_data = function(word, papers){
        papers_filtered = filter_by_word(papers, word)
        ## frequency absolute
        # already availible through the word_table
        ## frequency relative
        # already availible through the word_table
        ## avg citation count
        citation_count_avg = mean(papers_filtered$citedby.count)
        ## avg journal rank SJR
        SJR_avg = mean(papers_filtered$SJR)
        ## avg journal rank IPP
        IPP_avg = mean(papers_filtered$IPP)
        ## amount of different affiliations
        affiliation_count = papers_filtered$affilname %>% uniqueN
        return(data.table(citation_count_avg, SJR_avg, IPP_avg, affiliation_count))
      }
    })
    
    data = parLapply(cl, X = words, function(x) get_word_data(word = x, papers), papers)
    clusterEvalQ(cl, gc())
    stopCluster(cl)
  }
  if( !parallel ){
    data <- lapply(words, function(x) get_word_data(word = x, papers))
  }
  return(data.table(word_table, rbindlist(data)))
}

format_wordtable = function(word_table, threshold){
  if( is.null(word_table) ) return(NULL)
  word_table %>% arrange(desc(count_new)) %>% head(nrow(word_table)/100*threshold) %>% arrange(desc(ratio))
#   word_table %>% filter( count_new > threshold) %>% filter( count_ref > threshold) %>%
#     arrange(desc(ratio))
}

# make wordcloud with word_table
get_wordcloud = function(word_table, n = 100){
  if( is.null(word_table) ) return(NULL)
  word_table %<>% head(n)
  word_cloud <- wordcloud(words = word_table$word,
                         freq = as.integer(((((word_table$ratio - min(word_table$ratio)) / 
                                              (max(word_table$ratio) - min(word_table$ratio))) + 1)*2)^4), 
                         scale = c(5, 1), 
                         min.freq = 1, max.words=n, 
                         random.order = FALSE, 
                         colors = c("gray80", "gray50", hplightblue, hplightblue, hpdarkblue, hpdarkblue))
  return(word_cloud)  
}


