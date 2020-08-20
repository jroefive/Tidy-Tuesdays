
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
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

#select just lines in episodes written by a solo writer that has written more than one episode
so_writers_multi = c('Tim Hedrick', 'Michael Dante DiMartino', 'Joshua Hamilton', 'John O\'Bryan', 'Elizabeth Welch Ehasz', 'Aaron Ehasz')
solo_writers_multi <- subset(avatar, avatar$writer %in% so_writers_multi)

#Change to df that lists every word
words_tidy <- solo_writers_multi %>% 
  select(character,
         character_words, writer) %>% 
  filter(character != "Scene Description") %>% 
  group_by(writer) %>% 
  mutate(line_num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, character_words)

#Drop all mentions of speaking characters
characters = tolower(unique(avatar$character))
words_tidy <- subset(words_tidy,!(words_tidy$word %in% characters))

#Drop all stop_words
words_tidy <- words_tidy %>% 
  anti_join(stop_words)

#Group word counts by writer
writer_counts <- words_tidy %>%
  count(writer, word, sort = TRUE)


# Filter out only the top 10 words for each writer
writer_counts_table <- data.table(writer_counts, key='writer')
writer_counts_table <- writer_counts_table[, head(.SD, 10), by='writer']
writer_counts_table <- setorderv(writer_counts_table, 'n')
writer_counts_df <- as.data.frame(writer_counts_table)

# Plot
writer_counts_df %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col(color='#F15A50',
           fill="#2C6E91",) +
  facet_wrap(~writer, scales = 'free_y') +
  labs(title = "Most common words spoken in Avatar: The Last Airbender", subtitle = 'By Episode Writer', caption = 'Only episodes with a single writer who wrote more than one episode. Character names not included') +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight()


# Get and add sentiments
dat_bing <- get_sentiments("bing")
dat_sent <- words_tidy %>% inner_join(dat_bing)

# Counts of sentiments by writer
writer_sent <- dat_sent %>%
  count(writer, sentiment)

# Plot
writer_sent %>%
  mutate(sentiment = fct_reorder(sentiment, n)) %>%
  ggplot(aes(x = n, y = sentiment)) +
  geom_col(color='#F15A50',
           fill="#2C6E91",) +
  facet_wrap(~writer, scales = 'free_y') +
  labs(title = "Sentiment of words spoken in Avatar: The Last Airbender", subtitle = 'By Episode Writer', caption = 'Only episodes with a single writer who wrote more than one episode.') +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight()

