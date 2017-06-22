library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

setwd('~/Projekte/gntrm')

if( !('mdata' %in% ls()) ){
  source('scripts/teamfight_analysis.R')
}
if( !('pt' %in% ls() & 'correct' %in% ls()) ){
  source('scripts/teamgold_prediction.R')
}
rm(list=ls()[ls()!='tf' & ls()!='match' & ls()!='pt' & ls()!='correct' & ls()!='mdata'])

# count <- 0
p <- vector("list", 9) 
par(mfrow = c(3, 3))
for(n in c(1:9)){
# print(n)

id <- sample(match$match_id, 1)
freq <- 60
prediction <- c()
for(i in seq(60, match[match_id==id]$duration, freq)){
  current_gold <- pt[match_id==id & times<=i] %>% tail(1)
  last_teamfight <- tf[match_id==id & time<=i] %>% tail(1)
  
  rate_gold <- (correct[minute<=i/60] %>% tail(1))$rate
  relevance_gold <- (correct[minute<=i/60] %>% tail(1))$count
  if(!current_gold$gold_lead){
    rate_gold = 1 - rate_gold
  }
  
  rate_tf <- (mdata[time_delta<=i - last_teamfight$time] %>% tail(1))$ratio
  if(nrow(last_teamfight)>0){
    if(last_teamfight$deaths_1>last_teamfight$deaths_2){
      rate_tf <- 1 - rate_tf
    }
    if(last_teamfight$deaths_1==last_teamfight$deaths_2){
      rate_tf <- 0.5
    }
  
    rate_final <- (rate_gold * relevance_gold + rate_tf * 0.5)/(relevance_gold + 0.5)
  } else{
    rate_final <- rate_gold
  }
  prediction <- c(prediction, rate_final)
  # print(rate_final)
}
prediction <- data.table(time=seq(60, match[match_id==id]$duration, freq),
                         prediction=prediction)
correct_prediction <- match[match_id==id]$radiant_win == round(rate_final)
p[[n]] <- ggplot(prediction, aes(x=time, y=prediction)) +
  geom_line(color=ifelse(correct_prediction, 'darkgreen', 'darkred')) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  ggtitle(paste0('match_id=', id)) +
  theme(panel.grid.major = element_line(color='darkgray'),
        panel.grid.minor = element_line(color='gray'))

# count <- count + (match[match_id==id]$radiant_win == round(rate_final))
}

grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], ncol=3)
