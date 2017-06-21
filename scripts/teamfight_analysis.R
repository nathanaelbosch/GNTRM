# Analyze relevance of the teamfight data
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

rm(list=ls())
if( !('teamfights_min' %in% ls()) ){
  source('scripts/teamfight_data.R')
}
tf <- teamfights_min
source('scripts/outliers.R')
tf <- tf[!(match_id %in% outliers)]
rm(list=ls()[ls()!='tf'])


################################################################################
# First question: Do winning people win the last teamfight?
tf_last <- tf
tf_last <- tf_last[, maxtime:=max(time), by=match_id][time==maxtime]
tf_last$tf_win <- tf_last$deaths_2>tf_last$deaths_1
tf_last[, .(tf_win, radiant_win)]
win_perc <- sum(tf_last$tf_win==tf_last$radiant_win)/nrow(tf_last)
print(paste0(
  round(win_perc*100), "% of winning teams also won the last teamfight"))
# Leads to: How relevant is this number and last teamfights in general?

################################################################################
# 
if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
  tf_last <- merge(tf_last, match[, .(match_id, duration)], by='match_id')
}
tf_last$win_after_tf <- tf_last$tf_win==tf_last$radiant_win
tf_last$time_delta <- tf_last$duration-tf_last$time
ggplot(tf_last, aes(x=duration, y=time_delta, color=win_after_tf)) +
  geom_point(size=0.2) +
  scale_y_sqrt()

bin_size <- 10
tf_last$delta_bin <- tf_last$time_delta %>%
  cut(breaks=seq(0, max(tf_last$time_delta)+2*bin_size, bin_size)-bin_size)
hist_data <- tf_last %>% group_by(delta_bin) %>%
  summarize(ratio=sum((tf_win==radiant_win)==F)/n()) %>% data.table
ggplot(hist_data, aes(x=delta_bin, y=ratio)) +
  geom_bar(stat='identity') +
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