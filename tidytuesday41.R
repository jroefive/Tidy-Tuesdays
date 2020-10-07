library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)


tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

#Filter out the opneing round games
tournament = tournament %>% filter(tourney_finish!="OR")

#Re-order the finish categories for graphing
tournament$tourney_finish <- factor(tournament$tourney_finish, 
                                    levels = c('1st', '2nd',
                                               'RSF', 'RF', 
                                               'NSF', 'N2nd', 'Champ' ))


ggplot(tournament, aes(x=tourney_finish, y=reg_percent, color=seed)) + 
  geom_jitter(shape=16, position=position_jitter(0.25)) +
  geom_boxplot(alpha = 0.75, width=0.6, lwd = 1.2, fatten=0.7) +
  scale_color_gradientn(colours = rainbow(6)) +
  xlab('Round Lost') + ylab('Winning Percentage in Regular Season') +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_x_discrete(labels=c("1st" = "1st Round", "2nd" = "2nd Round", 
                            'RSF'='Sweet 16', 'RF'='Elite Eight', 
                            'NSF'='Final Four', 'N2nd'='Final (2nd Place)', 
                            'Champ'='National Champion')) +
  theme_bw() + 
  theme(axis.title.y = element_text(margin = 
                                      margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = 
                                      margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(title="Regular Seson Win Percentage of Women's NCAA Teams Who Lost in Each Round of National Tournament")