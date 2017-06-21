library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(slam)

if( !('chat' %in% ls()) ){
  chat <- fread('data/chat.csv')
}
if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
}


chat_ <- merge(chat, match, by='match_id')
chat_$radiant <- chat_$slot %in% c(0:4)
chat_$win <- ((chat_$radiant & chat_$radiant_win) |
                (!chat_$radiant & !chat_$radiant_win))
txt <- chat_ %>% 
  # sample_n(800000) %>%
  group_by(match_id, win) %>%
  summarize(text=paste(key, collapse=' ')) %>% data.table
# text <- chat$key

text_to_frequency_dt <- function(text){
  docs <- Corpus(VectorSource(text))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs %<>% tm_map(toSpace, "/") %>% tm_map(toSpace, "@") %>%
    tm_map(toSpace, "\\|")
  
  # Convert the text to lower case
  docs %<>% tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    # tm_map(removeWords, c("blabla1", "blabla2")) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  # dtm <- dtm_l - dtm_w
  dtm <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.table(word = names(v),freq=v)
  return(d)
}

d_lose <- text_to_frequency_dt(txt[win==F]$text)
d_win <- text_to_frequency_dt(txt[win==T]$text)
d <- merge(d_win, d_lose, by='word', all=T)
# d$ltow <- round(d$freq.y / d$freq.x * 50000)
d$ltow <- d$freq.y / d$freq.x
m <- mean(c(d$freq.x, d$freq.y), na.rm = T)
m <- 500
df <- d[freq.x>=m][freq.y>=m][order(-ltow)][!is.na(ltow)]

set.seed(1234)
wordcloud(words = df$word, freq = df$ltow^2,
          min.freq = 0,
          # max.words=100,
          random.order=FALSE,
          rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"),
          scale=c(2,.2))
wordcloud(words = df$word, freq = 1/df$ltow^2,
          min.freq = 0,
          # max.words=100,
          random.order=FALSE,
          rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"),
          scale=c(2,.2))
