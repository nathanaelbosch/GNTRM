library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

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
pt$gold_lead <- gold$gold_radiant>gold$gold_dire

gold <- data.table()
# for(i in c(5, 15, 30, 60)){
for(i in seq(60, mean(match$duration), 60)/60){
  min10 <- pt[times==60*i][, .(match_id, gold_radiant, gold_dire, radiant_win)]
  min10$min <- i
  gold <- rbind(gold, min10)
}

# 10min gold with win
p <- ggplot(gold, aes(x=gold_radiant, y=gold_dire, color=radiant_win)) +
  geom_jitter(stat='identity') +
  geom_line(data = data.frame(x = c(0,120000), y = c(0,120000)),
            aes(x = x, y = y), colour = "black", alpha=0.5) +
  ggtitle('Gold per team') +
  ylab('Dire Gold') +
  xlab('Radiant Gold') +
  scale_color_discrete(name='Radiant Win') +
  # facet_grid(~min) +
  theme_gray()
p

#####################################################################
# Frage: How "bad" is it to predict wins just with gold lead?


#####################################################################
# 
# r2 <- data.table(times=integer(), r_squared=numeric())
# coeffs <- data.table(
#   Intercept=numeric(),
#   gold_radiant=numeric(),
#   gold_dire=numeric())
# model <- lm(data=pt[times==i], formula=radiant_win~gold_radiant+gold_dire)
# for(i in seq(0, max(pt$times), 60)){
#   predictions <- model$coefficients[2]*pt[times==i]$gold_radiant + model$coefficients[3]*pt[times==i]$gold_radiant + model$coefficients[1]
#   # predictions <- predict(model, new_data=data.frame(pt[times==i][, .(gold_radiant, gold_dire)]))
#   errors <- sum((predictions>0.5) != pt[times==i]$radiant_win)
#   SStot <- sum(rep(0.5^2, nrow(pt[times==i])))
#   r2 <- 1 - errors/SStot
#   # r2 <- rbind(r2, data.table(times=i, r_squared=summary(model)$r.squared))
#   # coeffs <- rbind(coeffs,
#   #                 data.table(
#   #                   Intercept=model$coefficients[1],
#   #                   gold_radiant=model$coefficients[2],
#   #                   gold_dire=model$coefficients[3]))
# }
# p <- ggplot(r2, aes(x=times, y=r_squared)) +
#   geom_line()
# p
# 
# datapoint_count <- pt[, .(times, match_id)] %>%
#   group_by(times) %>% summarize(count=n()/50000) %>% data.table
# r2 <- merge(r2, datapoint_count, by='times')
# p <- ggplot(r2, aes(x=times, y=r_squared)) +
#   geom_line(color="red") +
#   geom_bar(stat='identity', aes(x=times, y=count), alpha=0.2) +
#   theme_gray()
# p
# 
# # p <- p + theme(
# #   panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
# #   # panel.grid.minor = element_blank(), 
# #   # panel.grid.major = element_blank(),
# #   plot.background = element_rect(fill = "transparent",colour = NA)
# # )
# # p
