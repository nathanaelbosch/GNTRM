library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(slam)

chat_ <- merge(chat, match, by='match_id')
chat_$radiant <- chat_$slot %in% c(0:4)
chat_$win <- ((chat_$radiant & chat_$radiant_win) |
                  (!chat_$radiant & !chat_$radiant_win))
# chat_ <- chat_[1:100000]
text_ <- chat_ %>% group_by(match_id, win) %>%
  summarize(text=paste(key, collapse=' ')) %>% data.table
# text <- chat$key
docs_w <- Corpus(VectorSource(text_[win==T]$text))
docs_l <- Corpus(VectorSource(text_[win==F]$text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_w %<>% tm_map(toSpace, "/") %>% tm_map(toSpace, "@") %>% tm_map(toSpace, "\\|")
docs_l %<>% tm_map(toSpace, "/") %>% tm_map(toSpace, "@") %>% tm_map(toSpace, "\\|")

# Convert the text to lower case
docs_w %<>% tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, c("blabla1", "blabla2")) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)
docs_l %<>% tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, c("blabla1", "blabla2")) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm_w <- TermDocumentMatrix(docs_w)
dtm_l <- TermDocumentMatrix(docs_l)
# dtm <- dtm_l - dtm_w
dtm <- dtm_w
dtm <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
