library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

setwd('~/Projekte/gntrm')

if( !('mdata' %in% ls()) ){
  source('scripts/teamfight_analysis.R')
}
if( !('pt' %in% ls() & 'correct' %in% ls()) ){
  source('scripts/teamgold_prediction.R')
}
source('scripts/mdata_combine.R')
rm(list=ls()[ls()!='tf' &
               ls()!='match' &
               ls()!='pt' &
               ls()!='correct' &
               ls()!='mdata'])

################################################################################
# Init finished, create prediction model for gold/exp
if( !('gold_model' %in% ls()) ){
  pt_ <- pt[match_id < 45000][, .(
  times, gold_t_0, gold_t_1, gold_t_2, gold_t_3, gold_t_4,
  gold_t_128, gold_t_129, gold_t_130, gold_t_131, gold_t_132,
  gold_lead, radiant_win)]
pt_ <- pt[match_id < 45000][, .(
  times, gold_lead, xp_lead, radiant_win)]
gold_model <- lm(pt_, formula=radiant_win~. + times*.)
# summary(model)$coefficients
}

# Create prediction model for teamfights
if( !('tf_model' %in% ls()) ){
  tf_ <- tf[match_id < 45000][, .(
  time, deaths_1, gold_delta_1, exp_delta_1,
  deaths_2, gold_delta_2, exp_delta_2, radiant_win)]
tf_$tf_win <- tf_$deaths_1<tf_$deaths_2
tf_model <- lm(tf_, formula=radiant_win~. + time*.)
# summary(tf_model)$coefficients
}

################################################################################

count <- 0
p <- vector("list", 9) 
# par(mfrow = c(3, 3))
for(n in c(1:9)){
print(n)

id <- sample(c(45000:49999), 1)
# freq <- 1
prediction <- c()

gold_predictions <- data.table(time=pt[match_id==id]$times,
                               gold_pred=predict(gold_model, pt[match_id==id]))
tf_predictions <- data.table(time=tf[match_id==id]$time,
                             tf_pred=predict(tf_model, tf[match_id==id]))
predictions <- merge(gold_predictions, tf_predictions, by='time', all=T)

predictions <- na.locf(predictions)
predictions[, prediction:=apply(
  data.table(gold_pred, tf_pred), 1, function(x){mean(x, na.rm=T)})]

correct_prediction <- match[match_id==id]$radiant_win == round(
  predictions$prediction %>% tail(1))
p[[n]] <- ggplot(predictions, aes(x=time, y=prediction)) +
  geom_line(color=ifelse(correct_prediction, 'darkgreen', 'darkred'), size=1) +
  coord_cartesian(ylim = c(0, 1)) +
  # ggtitle(paste0('match_id=', id)) +
  theme_bw() +
  theme(plot.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        panel.grid.major = element_line(color='darkgray'),
        panel.grid.minor = element_line(color='gray'))

# count <- count + correct_prediction
}

grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], ncol=3)

"
Prediction problem:
When filling NAs, I assume the prediction of the other programm did not change since!
Fix: Train the model add 'duration since' and train the model with that, while adding datapoints
"