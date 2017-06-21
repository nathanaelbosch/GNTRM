library(data.table)
library(magrittr)
library(dplyr)

rm(list=ls())

setwd('~/Projekte/gntrm')

files = c(
  'players',
  'match',
  'player_time',
  'objectives',
  'ability_upgrades',
  'purchase_log',
  'teamfights',
  'teamfights_players',
  'chat')
for(tfile in files){
  dt <- fread(paste0('data/', tfile, '.csv'))
  write.csv(dt[match_id<5000], file=paste0("small_data/", tfile, ".csv"), row.names=F)
  rm(list=ls())
}