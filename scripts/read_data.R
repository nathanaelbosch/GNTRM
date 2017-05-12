library(data.table)
library(magrittr)
library(dplyr)

rm(list=ls())

setwd('~/Projekte/gntrm')

##########################################################
# Out-game
players <- fread('data/players.csv')
match <- fread('data/match.csv')

# In-game
player_time <- fread('data/player_time.csv')
objectives <- fread('data/objectives.csv')
ability_upgrades <- fread('data/ability_upgrades.csv')
purchase_log <- fread('data/purchase_log.csv')
teamfights <- fread('data/teamfights.csv')
teamfights_players <- fread('data/teamfights_players.csv')
chat <- fread('data/chat.csv')

# Misc
patch_dates <- fread('data/patch_dates.csv')
ability_ids <- fread('data/ability_ids.csv')
cluster_regions <- fread('data/cluster_regions.csv')
hero_names <- fread('data/hero_names.csv')
item_ids <- fread('data/item_ids.csv')

# MMR model
# match_outcomes <- fread('data/match_outcomes.csv')      # Not the same 50k matches
# player_ratings <- fread('data/player_ratings.csv')      # Their test to model mmr
# test_labels <- fread('data/test_labels.csv')
# test_player <- fread('data/test_player.csv')