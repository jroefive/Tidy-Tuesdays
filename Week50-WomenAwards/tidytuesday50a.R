
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(tidytext)
library(gghighlight)
library(data.table)
library(forcats)
library(ggwordcloud)

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

#Drop unsung heros award
women = women[women$category != 'All',]

#Change to df that lists every word
words_tidy <- women %>% 
  ungroup() %>% 
  unnest_tokens(word, description)

#Drop all stop_words
words_tidy <- words_tidy %>% 
  anti_join(stop_words)

#Frequency table by category
word_df = as.data.frame(table(words_tidy$word, words_tidy$category))

#Rename columns
names(word_df) <- c('word', 'category', 'freq')

#Drop words that show up 2 or fewer times in a category
word_df = word_df[word_df$freq > 2,]

#Generate graph
word_cloud_graph = ggplot(word_df, aes(label = word, size=freq)) +
  geom_text_wordcloud_area(area_corr_power = 1, rm_outside = TRUE, shape = 'square') +
  facet_wrap(~category) +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(plot.margin = margin(l=5,r=5,t=5,b=5)) +
  scale_size_area(max_size = 24)

word_cloud_graph
