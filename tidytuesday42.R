library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gganimate)
library(ggpubr)

datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')


make_plot_x <- function(dataset_name) {
  away = datasaurus %>% filter(datasaurus$dataset == dataset_name)
  
  away_x_mean = mean(data.matrix(away[,'x']))
  
  away = away %>% mutate(x_dis_from_mean = x - away_x_mean)
  
  away = away[order(away$dataset, -away$x_dis_from_mean),]
  
  away <- tibble::rowid_to_column(away, "ID")
  
  away_pos = length(which(away$x_dis_from_mean > 0)) + 1
  away = away %>% mutate(second_plot = away$ID)
  
  away_pos_mean = mean(data.matrix(away[away$x_dis_from_mean>0,"x_dis_from_mean"]))
  away_neg_mean = mean(data.matrix(away[away$x_dis_from_mean<0,"x_dis_from_mean"]))
  
  away$third_plot <- ifelse(away$x_dis_from_mean > 0, away_x_mean + away_pos_mean, away_x_mean + away_neg_mean)
  away$color <- ifelse(away$x_dis_from_mean > 0, 'Greater Than Mean', 'Less Than Mean')
  
  away_long = away %>% pivot_longer(cols = c('y','second_plot'), names_to = 'plot_y', values_to = 'y')
  away_long = away_long %>% pivot_longer(cols = c('x','third_plot'), names_to = 'plot_x', values_to = 'x')
  away_long = away_long %>% filter(!(away_long$plot_y == 'y' & away_long$plot_x == 'third_plot'))
  away_long$plot <- ifelse(away_long$plot_y == 'y' & away_long$plot_x == 'x', 'First Plot', 
                           ifelse(away_long$plot_y == 'second_plot' & away_long$plot_x == 'x', 'Second Plot', 'Third Plot'))

  plot = ggplot(away_long, aes(x=x, y=y, color = color)) + geom_point() +
    geom_vline(xintercept = away_x_mean) + 
    geom_hline(yintercept = away_pos) + 
    annotate("rect", xmin = away_x_mean, xmax = away_x_mean + away_pos_mean, ymin = 0, ymax = away_pos, alpha = .2) + 
    annotate("rect", xmin = away_x_mean + away_neg_mean, xmax = away_x_mean, ymin = away_pos, ymax = 142, alpha = .2) +
    annotate(geom="text", x=45, y=-3, label=paste("Avg Dist Above Mean ", round(away_pos_mean,2)), color="red") + 
    annotate(geom="text", x=38, y=55, label=paste(away_pos-1," Points Above Mean"), color="red") +
    annotate(geom="text", x=35, y= 25, label=paste(away_pos-1, "*", round(away_pos_mean,2), '= ', round((away_pos-1)*away_pos_mean)), color='red') +
    annotate(geom="text", x=35, y= 20, label="Total Dist Above Mean", color="red") +
    annotate(geom="text", x=60, y=147, label=paste("Avg Dist Below Mean ", round(away_neg_mean,2)), color="blue") + 
    annotate(geom="text", x=75, y=75, label=paste(143-away_pos," Points Below Mean"), color="blue") +
    annotate(geom="text", x=80, y= 110, label=paste(143-away_pos, "*", round(away_neg_mean,2), '= ', round((143-away_pos)*away_neg_mean)), color='blue') +
    annotate(geom="text", x=80, y= 105, label="Total Dist Below Mean", color="blue")
  
  returns = list('plot' = plot, 'df'=away_long)
  return(returns)}

make_plot_y <- function(dataset_name) {
  away = datasaurus %>% filter(datasaurus$dataset == dataset_name)
  
  away_y_mean = mean(data.matrix(away[,'y']))
  
  away = away %>% mutate(y_dis_from_mean = y - away_y_mean)
  
  away = away[order(away$dataset, -away$y_dis_from_mean),]
  
  away <- tibble::rowid_to_column(away, "ID")
  
  away_pos = length(which(away$y_dis_from_mean > 0)) + 1
  away = away %>% mutate(second_plot = away$ID)
  
  away_pos_mean = mean(data.matrix(away[away$y_dis_from_mean>0,"y_dis_from_mean"]))
  away_neg_mean = mean(data.matrix(away[away$y_dis_from_mean<0,"y_dis_from_mean"]))
  
  away$third_plot <- ifelse(away$y_dis_from_mean > 0, away_y_mean + away_pos_mean, away_y_mean + away_neg_mean)
  away$color <- ifelse(away$y_dis_from_mean > 0, 'Greater Than Mean', 'Less Than Mean')
  
  away_long = away %>% pivot_longer(cols = c('x','second_plot'), names_to = 'plot_x', values_to = 'x')
  away_long = away_long %>% pivot_longer(cols = c('y','third_plot'), names_to = 'plot_y', values_to = 'y')
  away_long = away_long %>% filter(!(away_long$plot_x == 'x' & away_long$plot_y == 'third_plot'))
  away_long$plot <- ifelse(away_long$plot_x == 'x' & away_long$plot_y == 'y', 'First Plot', 
                           ifelse(away_long$plot_x == 'second_plot' & away_long$plot_y == 'y', 'Second Plot', 'Third Plot'))
  
  plot = ggplot(away_long, aes(x=x, y=y, color = color)) + geom_point() +
    geom_hline(yintercept = away_y_mean) + 
    geom_vline(xintercept = away_pos) + 
    annotate("rect", ymin = away_y_mean, ymax = away_y_mean + away_pos_mean, xmin = 0, xmax = away_pos, alpha = .2) + 
    annotate("rect", ymin = away_y_mean + away_neg_mean, ymax = away_y_mean, xmin = away_pos, xmax = 142, alpha = .2) +
    annotate(geom="text", x=115, y=62, label="Avg Dist Above Mean", color="red") + 
    annotate(geom="text", x=95, y=57, label=round(away_pos_mean,2), color="red") +
    annotate(geom="text", x=38, y=72, label=paste(away_pos-1," Points Above Mean"), color="red") +
    annotate(geom="text", x=115, y= 75, label=paste(away_pos-1, "*", round(away_pos_mean,2), '= ', round((away_pos-1)*away_pos_mean)), color='red') +
    annotate(geom="text", x=115, y= 70, label="Total Dist Above Mean", color="red") +
    annotate(geom="text", x=40, y=35, label=paste("Avg Dist Below Mean ", round(away_neg_mean,2)), color="blue") + 
    annotate(geom="text", x=118, y=18, label=paste(143-away_pos," Points Below Mean"), color="blue") +
    annotate(geom="text", x=35, y= 25, label=paste(143-away_pos, "*", round(away_neg_mean,2), '= ', round((143-away_pos)*away_neg_mean)), color='blue') +
    annotate(geom="text", x=35, y= 20, label="Total Dist Below Mean", color="blue")
  
  returns = list('plot' = plot, 'df'=away_long)
  return(returns)}

#plot_away = make_plot('away')
plot_dino = make_plot_y('h_lines')

#plot_dino$plot


##Animate
#anim = plot_dino$plot + transition_states(plot_dino$df$plot, transition_length = 2, state_length = 1) +
#  shadow_mark(alpha = 0.3, size = 0.5) +
#  shadow_wake(wake_length = 0.05, alpha = 0.3)


# Video output
#animate(
#  anim + enter_fade(),
#  renderer = gifski_renderer())

#anim_save("H_lines_y_avg.gif")

#make_plot_facet_x <- function() {

away = datasaurus 

away_x_mean = mean(data.matrix(away[,'x']))

away = away %>% mutate(x_dis_from_mean = x - away_x_mean)

away = away[order(away$dataset, -away$x_dis_from_mean),]

away <- tibble::rowid_to_column(away, "ID")
away$ID2 = away$ID %% 142
away$ID2 = ifelse(away$ID2 == 0, 142, away$ID2)

away = away %>%
  group_by(dataset) %>% mutate(away_pos = length(which(x_dis_from_mean>0))+1)

away$second_plot = away$ID2
away$color <- ifelse(away$x_dis_from_mean > 0, 'Greater Than Mean', 'Less Than Mean')

away = away %>% group_by(dataset, color) %>%
  mutate(away_from_mean = mean(x_dis_from_mean))

away$third_plot <- away_x_mean + away$away_from_mean

away_long = away %>% pivot_longer(cols = c('y','second_plot'), names_to = 'plot_y', values_to = 'y')

away_long = away_long %>% pivot_longer(cols = c('x','third_plot'), names_to = 'plot_x', values_to = 'x')

away_long = away_long %>% filter(!(plot_y == 'y' & plot_x == 'third_plot'))
away_long$plot <- ifelse(away_long$plot_y == 'y' & away_long$plot_x == 'x', 'First Plot', 
                         ifelse(away_long$plot_y == 'second_plot' & away_long$plot_x == 'x', 'Second Plot', 'Third Plot'))

plot_all_x = ggplot(away_long, aes(x=x, y=y, color = color)) + geom_point() +
  geom_vline(xintercept = away_x_mean) + facet_wrap(~dataset)

##Animate
#anim = plot_all_x + transition_states(away_long$plot, transition_length = 2, state_length = 1) +
#  shadow_mark(alpha = 0.3, size = 0.5) +
#  shadow_wake(wake_length = 0.05, alpha = 0.3)


# Video output
#animate(
#  anim + enter_fade(),
#  renderer = gifski_renderer(), height = 800, width =800)

#anim_save("all_x_avg.gif")

away = datasaurus 

away_y_mean = mean(data.matrix(away[,'y']))

away = away %>% mutate(y_dis_from_mean = y - away_y_mean)

away = away[order(away$dataset, -away$y_dis_from_mean),]

away <- tibble::rowid_to_column(away, "ID")
away$ID2 = away$ID %% 142
away$ID2 = ifelse(away$ID2 == 0, 142, away$ID2)

away = away %>%
  group_by(dataset) %>% mutate(away_pos = length(which(y_dis_from_mean>0))+1)

away$second_plot = away$ID2
away$color <- ifelse(away$y_dis_from_mean > 0, 'Greater Than Mean', 'Less Than Mean')

away = away %>% group_by(dataset, color) %>%
  mutate(away_from_mean = mean(y_dis_from_mean))

away$third_plot <- away_y_mean + away$away_from_mean

away_long = away %>% pivot_longer(cols = c('x','second_plot'), names_to = 'plot_x', values_to = 'x')

away_long = away_long %>% pivot_longer(cols = c('y','third_plot'), names_to = 'plot_y', values_to = 'y')

away_long = away_long %>% filter(!(plot_x == 'x' & plot_y == 'third_plot'))
away_long$plot <- ifelse(away_long$plot_x == 'x' & away_long$plot_y == 'y', 'First Plot', 
                         ifelse(away_long$plot_x == 'second_plot' & away_long$plot_y == 'y', 'Second Plot', 'Third Plot'))

plot_all_y = ggplot(away_long, aes(x=x, y=y, color = color)) + geom_point() +
  geom_hline(yintercept = away_y_mean) + facet_wrap(~dataset)

##Animate
anim = plot_all_y + transition_states(away_long$plot, transition_length = 2, state_length = 1) +
  shadow_mark(alpha = 0.3, size = 0.5) +
  shadow_wake(wake_length = 0.05, alpha = 0.3)


# Video output
animate(
  anim + enter_fade(),
  renderer = gifski_renderer(), height = 800, width =800)

anim_save("all_y_avg.gif")
