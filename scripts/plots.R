library(anytime)
library(ggplot2)

############################################################
# Games per Date
match$date <- match$start_time %>% anydate
matches_per_day <- match %>% group_by(date) %>% count

p <- ggplot(matches_per_day, aes(x=date, y=n)) +
  geom_bar(stat='identity') +
  ggtitle('Games per Day') +
  ylab('Number of games') +
  xlab('Date') +
  theme_bw()
p

###########################################################
# Regions
matches_per_region <- merge(match, cluster_regions)
matches_per_region %<>% group_by(region) %>% count %>% data.table
matches_per_region$region %<>% factor(levels=sort(matches_per_region$region, decreasing=T))
p <- ggplot(matches_per_region, aes(x=region, y=n)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle('Games per Region') +
  ylab('Number of games') +
  xlab('Server Region') +
  theme_bw()
p

##########################################################
# Heroes played
# players$radiant <- players$player_slot %in% c(0,1,2,3,4)
# players <- merge(players, match)
# players$win <- ((players$radiant & players$radiant_win) |
#                   (!players$radiant & !players$radiant_win))
heroes_ <- merge(players, hero_names, by='hero_id')
heroes <- heroes_ %>% group_by(localized_name) %>%
  summarize(count=n(), win_percentage=mean(win)*100) %>% data.table()
heroes$play_percentage <- heroes$count/50000*100
setkey(heroes, play_percentage)
heroes$name <- factor(heroes$localized_name, levels=heroes$localized_name)
# p <- ggplot(heroes, aes(x=names, y=play_percentage, fill=win_percentage)) +
#   geom_bar(stat='identity') +
#   coord_flip() +
#   ggtitle('Games per Region') +
#   ylab('Number of games') +
#   xlab('Server Region') +
#   theme_bw()
# p
p <- ggplot(heroes, aes(x=play_percentage, y=win_percentage, fill=name)) +
  geom_jitter() +
  guides(fill=F) +
  ylab('Win %') +
  xlab('Play %') +
  ggtitle('Hero occurrence') +
  theme_bw()
p

##########################################################
# Example Game
id <- sample(match$match_id, 1)
match[match_id==id] %>% dim
players[match_id==id] %>% dim
player_time[match_id==id] %>% dim
objectives[match_id==id] %>% dim
ability_upgrades[match_id==id] %>% dim
purchase_log[match_id==id] %>% dim
teamfights[match_id==id] %>% dim
teamfights_players[match_id==id] %>% dim
chat[match_id==id] %>% dim

#########################################################
# Match durations
p <- ggplot(data=match, aes(x=duration)) +
  geom_histogram(binwidth=60)
p
