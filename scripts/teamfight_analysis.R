# Analyze relevance of the teamfight data
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

if( !('teamfights_min' %in% ls()) ){
  source('scripts/teamfight_data.R')
}
tf <- teamfights_min
source('scripts/outliers.R')
tf <- tf[!(match_id %in% outliers)]
rm(list=ls()[ls()!='tf'])

###############################################################################
# Group teamfights together that happen quickly after another
tf$diff <- c(-1000, tf$start[-1] - tf$time[-length(tf$time)])
allowed_pause <- 180
indizes <- which(tf$diff<allowed_pause & tf$diff>0)
while(length(indizes)>0){
  tf[, start2:=shift(start)]
  tf[, deaths_12:=deaths_1+shift(deaths_1)]
  tf[, deaths_22:=deaths_2+shift(deaths_2)]
  tf[, gold_delta_12:=gold_delta_1+shift(gold_delta_1)]
  tf[, gold_delta_22:=gold_delta_2+shift(gold_delta_2)]
  tf[, exp_delta_12:=exp_delta_1+shift(exp_delta_1)]
  tf[, exp_delta_22:=exp_delta_2+shift(exp_delta_2)]
  
  tf[indizes, start:=start2]
  tf[indizes, deaths_1:=deaths_12]
  tf[indizes, deaths_2:=deaths_22]
  tf[indizes, gold_delta_1:=gold_delta_12]
  tf[indizes, gold_delta_2:=gold_delta_22]
  tf[indizes, exp_delta_1:=exp_delta_12]
  tf[indizes, exp_delta_2:=exp_delta_22]

  tf <- tf[-(indizes-1)]
  tf$diff <- c(-1000, tf$start[-1] - tf$time[-length(tf$time)])
  indizes <- which(tf$diff<30 & tf$diff>0)
}
tf[, c('diff', 'start2', 'deaths_12', 'deaths22', 'gold_delta_12',
       'gold_delta_22', 'exp_delta_12', 'exp_delta_22') := NULL]

################################################################################
# First question: Do winning people win the last teamfight?
tf_last <- tf
tf_last <- tf_last[, maxtime:=max(time), by=match_id][time==maxtime]
tf_last$tf_win <- tf_last$deaths_2>=tf_last$deaths_1
tf_last[, .(tf_win, radiant_win)]
win_perc <- sum(tf_last$tf_win==tf_last$radiant_win)/nrow(tf_last)
print(paste0(
  round(win_perc*100), "% of winning teams also won the last teamfight"))
# Leads to: How relevant is this number and last teamfights in general?

################################################################################
# 
if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
}
tf_last <- merge(tf_last, match[, .(match_id, duration)], by='match_id')
tf_last$win_after_tf <- tf_last$tf_win==tf_last$radiant_win
tf_last$time_delta <- tf_last$duration-tf_last$time
# ggplot(tf_last, aes(x=duration, y=time_delta, color=win_after_tf)) +
#   geom_point(size=0.2) +
#   scale_y_log10()

rounding_number <- 10
mdata <- tf_last %>% group_by(time_delta=round(time_delta/rounding_number)) %>%
  summarize(ratio=sum((tf_win==radiant_win)==T)/n(), number=n()) %>% data.table
mdata$time_delta <- mdata$time_delta*rounding_number
ggplot(mdata, aes(x=time_delta, y=ratio)) +
  geom_bar(stat='identity') +
  scale_x_continuous(
    'Time difference between last teamfight and end of game',
     breaks = seq(0, max(mdata$time_delta), 100)) +
  scale_y_continuous('Win Probability after won teamfight') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Teamfights look relevant !
# * Even for larger deltas there is a good ratio, but sample size gets small there
# * The general lead attained by the teamfight should also be visible from gold, exp and items
# * We especially want to use the momentum the teamfight creates for prediction
# => The curve between 0 and 150 looks highly relevant, that's what we aim for!

# Todo:
# * Why does it go down for very small delta?
# * What model could we use and how?
#   + challenge: Don't forget older teamfights
#   + lstm: able to remember ~3-5 steps (roughly)
# * Use more complete data than just the summarized outcome of the tf