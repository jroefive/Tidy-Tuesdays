
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(tidyverse)
library(caret)
library(ranger)
library(tidylog)
library(dataedu)


memory.limit(size=56000)
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs_predict <- spotify_songs %>%
  select(track_popularity, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms)

spotify_songs_predict <- 
  spotify_songs_predict %>% 
  mutate_if(is.character, as.factor)

spotify_songs_predict <- spotify_songs_predict[sample(nrow(spotify_songs_predict), 5000), ]

set.seed(2020)
trainIndex <- createDataPartition(spotify_songs_predict$track_popularity,
                                  p = .8, 
                                  list = FALSE,
                                  times = 1)

spotify_songs_predict <- 
  spotify_songs_predict %>% 
  mutate(temp_id = 1:5000)

df_train <- 
  spotify_songs_predict %>% 
  filter(temp_id %in% trainIndex)

df_test <- 
  spotify_songs_predict %>% 
  filter(!temp_id %in% trainIndex)

spotify_songs_predict <- 
  spotify_songs_predict %>% 
  select(-temp_id)

df_train <- 
  df_train %>% 
  select(-temp_id)

df_test <- 
  df_test %>% 
  select(-temp_id)

df_train <- na.omit(df_train)
df_test <- na.omit(df_test)

set.seed(2020)

rf_fit2_imp <-
  train(
    track_popularity ~ .,
    data = df_train,
    method = "ranger",
    importance = "permutation"
  )

varImp(rf_fit2_imp) %>%
  pluck(1) %>%
  rownames_to_column("var") %>%
  ggplot(aes(x = reorder(var, Overall), y = Overall)) +
  geom_col(fill = dataedu_colors("darkblue")) +
  coord_flip() +
  theme_dataedu() + 
  labs(title = 'Feature Importance for Track Popularity')
