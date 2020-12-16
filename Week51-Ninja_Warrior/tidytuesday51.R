library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)

#Get data
ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

#Turn data into frequency table of obstacles and rename columns
most_common_obstacles_df = as.data.frame(table(ninja_warrior$obstacle_name))
names(most_common_obstacles_df) <- c('obstacle', 'freq')

#Get subset of df for join and graph labels
most_common_obstacles_df_top = most_common_obstacles_df %>% filter(most_common_obstacles_df$freq > 11)

#Get frequency table of obstacle frequenceis
freq_count_df = as.data.frame(table(most_common_obstacles_df$freq))

#Merge with subset df with only obstacle names that have >11 appearances
freq_count_label_df = merge(x=freq_count_df, y=most_common_obstacles_df_top, by.x='Var1', by.y='freq', all.x=TRUE)

ggplot(data=freq_count_label_df, aes(x=factor(Var1, levels=0:87), y=Freq)) + 
  geom_bar(stat='identity', width=0.8) +
  geom_text(aes(label=obstacle, angle=90, hjust=-0.1, vjust=0.2)) +
  scale_x_discrete(breaks=obst_counts, labels=obst_counts, drop=F) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
  xlab('# of Appearances') + ylab('# of Obstacles') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
