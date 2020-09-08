library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(tidytext)
library(gghighlight)
library(data.table)
library(forcats)

#Upload Data
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

#Join emotions with script
friends_join = left_join(friends, friends_emotions)

#Keep just the main characters
main_chars = c("Monica Geller","Joey Tribbiani","Chandler Bing","Phoebe Buffay","Ross Geller","Rachel Green")
main_char_df = subset(friends_join, friends_join$speaker %in% main_chars)

#Change to df that lists every word
words_tidy <- main_char_df %>% 
  select(speaker,
         season, episode, scene, utterance, emotion, text) %>% 
  group_by(speaker) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

#Drop all stop_words
words_tidy <- words_tidy %>% 
  anti_join(stop_words)

# Get and add sentiments
dat_bing <- get_sentiments('afinn')
dat_sent <- words_tidy %>% inner_join(dat_bing)

#Find top words for each sentiment value to add to caption
val_counts = dat_sent %>% group_by(value) %>% filter(value==5) %>%
  count(word, value)
val_counts = val_counts[order(-val_counts$n),]
val_counts

# Counts of sentiments by speaker and season
speaker_sent <- dat_sent %>% group_by(season) %>%
  count(speaker, value)

# Plot
speaker_sent %>%
  ggplot(aes(y = n, x = season, fill=as.factor(value))) +
  geom_col(position = 'fill') +
  facet_grid(. ~speaker) +
  labs(x='Season', y='Percentage of sentiment words at each level', fill='Sentiment Value', title = "Sentiment of words spoken by main characters in Friends", subtitle = 'Afinn sentiment values', caption = 'Most Common Word for Each Value: (5)thrilled, (4)wow, (3)love, (2)fine, (1)yeah, (-1)stop, (-2)wrong, (-3)bad, (-4)hell, (-5)bitch') +
  theme(legend.position = 'right') +
  scale_fill_brewer(palette="RdYlBu")
