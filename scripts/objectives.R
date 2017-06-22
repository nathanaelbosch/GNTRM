library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

if( !('match' %in% ls()) ){
  match <- fread('data/match.csv')
}
if( !('objectives' %in% ls()) ){
  match <- fread('data/objectives.csv')
}

objectives[subtype=='CHAT_MESSAGE_BARRACKS_KILL']$radiant <- objectives[
  subtype=='CHAT_MESSAGE_BARRACKS_KILL']$value <= 2^5
