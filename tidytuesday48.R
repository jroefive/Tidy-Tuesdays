
library(ggplot2)
library(tidyr)

#upload data
hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

#Change rating and highpoint to dbl for graphing
hike_data$rating = as.numeric(as.character(hike_data$rating))
hike_data$highpoint = as.numeric(as.character(hike_data$highpoint))

#Drop all 0.00 ratings, assuming they never got rated
hike_data = hike_data[!hike_data$rating == 0.00,]

#Unnest features into their own column
hike_data = hike_data %>%    
  mutate_if(is.list, simplify_all) %>%   
  unnest(cols=c(features)) 

#Plot
ggplot(hike_data, aes(x=features, y=rating, color=highpoint)) + 
  geom_jitter(shape=16, position=position_jitter(0.25)) +
  geom_boxplot(alpha = 0.5, width=0.6, lwd = 1.2, fatten=0.7) +
  scale_color_gradientn(colours = rainbow(6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  xlab('Feature Listed for Hike') + ylab('Rating of Hike')+
  theme(axis.title.y = element_text(size = 15, margin = 
                                      margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(size = 16, margin = 
                                      margin(t = 10, r = 0, b = 0, l = 0))) 

