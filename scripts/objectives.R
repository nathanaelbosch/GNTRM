library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
}
if( !('objectives' %in% ls()) ){
  objectives <- fread('data/objectives.csv')
}

objectives$subtype %<>% factor
objectives$radiant <- NA
objectives[subtype=='CHAT_MESSAGE_BARRACKS_KILL']$radiant <- objectives[
  subtype=='CHAT_MESSAGE_BARRACKS_KILL']$value <= 2^5

if( !('radiant_win' %in% colnames(objectives)) ){
  objectives <- merge(objectives, match[, .(match_id, radiant_win)], by='match_id')
}

# Radiant is 1, Dire is 2
objectives$firstblood <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_FIRSTBLOOD",
  ifelse(objectives$slot<5, 1, 2),
  NA)
objectives$tower_kill <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_TOWER_KILL",
  ifelse(objectives$slot<5, 1, 2),
  NA)
# Roshan: Team 2 -> radiant? TO CHECK!
objectives$roshan_kill <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_ROSHAN_KILL",
  ifelse(objectives$team==2, 1, 2),
  NA)
objectives$barracks_kill <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_BARRACKS_KILL",
  ifelse(objectives$value <= 2^5, 1, 2),
  NA)
objectives$aegis <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_AEGIS",
  ifelse(objectives$player1 < 5, 1, 2),
  NA)
objectives$aegis_stolen <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_AEGIS_STOLEN",
  ifelse(objectives$player1 < 5, 1, 2),
  NA)
objectives$tower_deny <- ifelse(
  objectives$subtype == "CHAT_MESSAGE_TOWER_DENY",
  ifelse(objectives$player1 < 5, 1, 2),
  NA)
