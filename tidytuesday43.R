
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gganimate)
library(ggpubr)
library(statebins)
library(geofacet)

#import data
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')


#Create 5 year eras (probably a better way to do this)
beer_awards$era = ifelse(beer_awards$year < 1991, '1987-1990', 
                         ifelse(beer_awards$year < 1996, '1991-1995',
                                ifelse(beer_awards$year < 2001, '1996-2001',
                                       ifelse(beer_awards$year < 2006, '2001-2006',
                                              ifelse(beer_awards$year < 2011, '2006-2010',
                                                     ifelse(beer_awards$year < 2016, '2011-2015',
                                                            '2016-2020'))))))

#Fix messy state titles and reorder medals
beer_awards = beer_awards %>% mutate(state=recode(state, "Ak"="AK", "wa"="WA")) %>%
  mutate(medal=factor(medal, levels=c("Gold","Silver","Bronze")))

#create a freq table for state and era, change to df
state_totals = table(beer_awards$state, beer_awards$era)
state_totals_df = as.data.frame(state_totals)
names(state_totals_df) = c('state', 'era', 'count')

#Change count to percents then bucket percents into 4% intervals
state_totals_df = state_totals_df %>% group_by(era) %>% mutate(percent = count/sum(count)) %>%
  mutate(share = cut(percent, breaks = 6, labels = c("0-4%", "4-8%", "8-12%", "12-16%", '16-20%', '20-24%')))

#Change interval to 0 for no awards to make it easier to see when states didn't win any awards
state_totals_df$share = ifelse(state_totals_df$percent == 0, '0%', as.character(state_totals_df$share))

#Reorder factors for graphing
state_totals_df$share <- factor(state_totals_df$share, levels = c("0%", "0-4%", "4-8%", "8-12%", "12-16%", '16-20%', '20-24%'))

#Original plot with facet geo to try to show a bar for each type of medal
#plot = state_totals_df %>%
#  ggplot(aes(x=medal, y=count, fill=medal)) +
#  geom_bar(stat='identity') +
#  facet_geo(~state) +
#  scale_fill_manual("legend", values = c("Gold" = "gold2", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")) +
#  ylab('Total # of Medals') +
#  theme_bw() +
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(),
#        legend.position='none') +
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#statebins plot
plot = state_totals_df %>%
  ggplot(aes(state = as.character(state), fill = share)) +
  geom_statebins() + 
  theme_statebins() +
  scale_fill_manual(values=c("#f3efe0", "#ffeeab", "#FFC971", "#FFB627", "#FF9505", "#E2711D", "#CC5803"))

##Animate
anim = plot + transition_states(era, transition_length = 3, state_length = 3) +
  labs(title = 'Percent of All Great American Beer Festival Medals Won in: {closest_state}') + 
  theme(plot.title = element_text(size = 16))


# Video output
animate(
  anim,
  renderer = gifski_renderer(), height = 800, width =1000)

anim_save("gaba_statebins.gif")