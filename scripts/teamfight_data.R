"
Main idea is to create a single data.frame with which we can work well
So far I made a minimal Version, containing team deaths and gold delta
"
library(data.table)
library(magrittr)
library(dplyr)

teamfights <- fread('data/teamfights.csv')
teamfights_players <- fread('data/teamfights_players.csv')
match <- fread('data/match.csv')

teamfights[, tf_nr := seq_len(.N), by = match_id]
teamfights[, time:=last_death]
teamfights_players[, tf_nr := seq_len(.N), by = .(match_id, player_slot)]

tf <- merge(teamfights[, .(match_id, time, tf_nr, start)],
            teamfights_players, by=c('match_id', 'tf_nr'))
tf$team <- ifelse(tf$player_slot < 5, 1, 2)
df <- tf %>% group_by(match_id, time, team, start) %>% 
  summarise(
    deaths=sum(deaths),
    gold_delta=sum(gold_delta),
    exp_delta=sum(xp_end-xp_start)) %>% 
  data.table()
df <- merge(
  df[team==1][, team:=NULL],
  df[team==2][, team:=NULL],
  by=c('match_id', 'time', 'start'),
  suffixes=c('_1', '_2'))

teamfights_min <- merge(
  df,
  match[, .(match_id, radiant_win)],
  by='match_id')
teamfights_min
