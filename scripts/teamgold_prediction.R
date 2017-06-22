library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
}
source('scripts/outliers.R')
match <- match[!(match_id %in% outliers)]


init <- function(){
  if( !('match' %in% ls()) ){
    match <- fread('data/match.csv')
  }
  if( !('player_time' %in% ls()) ){
    player_time <- fread('data/player_time.csv')
  }
  pt <- player_time
  pt$gold_radiant <- (pt$gold_t_0
                      + pt$gold_t_1
                      + pt$gold_t_2
                      + pt$gold_t_3
                      + pt$gold_t_4)
  pt$gold_dire <- (pt$gold_t_128
                   + pt$gold_t_129
                   + pt$gold_t_130
                   + pt$gold_t_131
                   + pt$gold_t_132)
  pt <- merge(pt, match[, .(match_id, radiant_win)], by='match_id')
  pt$gold_lead <- pt$gold_radiant>pt$gold_dire
  pt$minute <- pt$times/60
  return(pt)
}
pt <- init()

############################################################################
gold <- data.table()
for(i in c(5, 15, 30, 60)){
# for(i in seq(60, mean(match$duration), 60)/60){
  min10 <- pt[times==60*i][, .(match_id, gold_radiant, gold_dire, radiant_win)]
  min10$min <- i
  gold <- rbind(gold, min10)
}

gold$min_ <- paste(gold$min, "min") %>% factor %>% relevel("5 min")
# levels(gold$min_) <- unique(gold$min_)

# 10min gold with win
ggplot(gold, aes(x=gold_radiant, y=gold_dire, color=radiant_win)) +
  geom_jitter(stat='identity') +
  # geom_line(data = data.frame(x = c(0,120000), y = c(0,120000)),
  #           aes(x = x, y = y), colour = "black", alpha=0.5) +
  ggtitle('Gold per team') +
  ylab('Team 2 Gold') +
  xlab('Team 1 Gold') +
  scale_color_discrete(name='Team 1 wins') +
  facet_grid(~min_) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#####################################################################
# Frage: How "good" is it to predict wins just with gold lead?
misclass <- c()
correct <- c()
for(i in seq(1, max(match$duration)/60)){
  pt_ <- pt[minute==i]
  misclass <- c(misclass, sum(pt_$gold_lead != pt_$radiant_win)/nrow(pt_))
  correct <- c(correct, sum(pt_$gold_lead == pt_$radiant_win)/nrow(pt_))
}

correct <- data.table(minute=c(1:length(correct)),
                      rate=correct)
datapoint_count <- pt[, .(minute, match_id)] %>%
  group_by(minute) %>% summarize(count=n()/50000) %>% data.table
correct <- merge(correct, datapoint_count, by='minute')
p <- ggplot(correct, aes(x=minute, y=rate)) +
  geom_line(color="red") +
  geom_bar(stat='identity', aes(x=minute, y=count), alpha=0.2) +
  ggtitle('Win-prediction by gold lead') +
  ylab('Accuracy') +
  xlab('Minute') +
  theme_gray() +
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p
