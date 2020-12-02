
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(ggmap)
library(maps)
library(mapdata)

#Import Data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

#Register google API key
register_google(key = "", write = TRUE)

#Drop shelters with 0 capacity
shelters = shelters %>% filter(capacity > 0)

#Change to date from datetime
shelters$date_op = as.Date(shelters$occupancy_date)

#Get sector totals by day
shelter_by_date = shelters %>% group_by(date_op, sector) %>% 
  summarize(occupancy = sum(occupancy), capacity_date = sum(capacity))

#Get column for unused capacity
shelter_by_date$unused_capacity = shelter_by_date$capacity_date - shelter_by_date$occupancy

#Pivot longer to have a row for occupancy and unused capacity
shelter_by_date_long = shelter_by_date %>% 
  pivot_longer(names_to = 'type', cols = c(occupancy, unused_capacity), 
               values_to = "people")

#Combine sector and occupancy type for bar labels
shelter_by_date_long$bar = paste(shelter_by_date_long$sector, 
                                 shelter_by_date_long$type, sep=' - ')

#Reset order of graph for better visual
shelter_by_date_long$bar = factor(shelter_by_date_long$bar, levels = 
                                    c('Families - unused_capacity', 'Families - occupancy', 
                                      'Co-ed - unused_capacity', 'Co-ed - occupancy',
                                      'Men - unused_capacity', 'Men - occupancy',
                                      'Women - unused_capacity', 'Women - occupancy', 
                                      'Youth - unused_capacity', 'Youth - occupancy'))
#Set color scheme
color_scheme = c('#87677B', '#463641', 
                 '#9A7AA0', '#644C6B', 
                 '#8D94BA', '#41476C', 
                 '#A0CFDe', '#3E7D87', 
                 '#B4EDD2', '#31AA7A')

#Plotting
ggplot(data=shelter_by_date_long, aes(x=date_op, y=people, fill=bar)) +
  geom_bar(stat="identity") +
  theme_bw() +
  scale_fill_manual(values = color_scheme) +
  xlab('Date') + ylab('# of People') 

#Get avergae occupancy and capacity of all shelters for map graph
shelters_by_avg_cap = shelters %>% group_by(shelter_name) %>% 
  summarize(avg_occ = mean(occupancy), avg_cap = mean(capacity))

#Get lon and lat for all shelters
unique_shelters = shelters %>% distinct(shelter_name, .keep_all = TRUE)

unique_shelters$address = paste(unique_shelters$shelter_address, 
                                unique_shelters$shelter_city, sep=", ")

unique_shelters = mutate_geocode(unique_shelters, address)

#Join lon and lat to average occupancy and capacity df
shelters_by_avg_cap = left_join(shelters_by_avg_cap, unique_shelters, 
                                by="shelter_name")

#Set color scheme
color_scheme2 = c('#463641', '#644C6B', '#41476C', '#3E7D87', '#31AA7A')

#Draw map box
tor_box <- get_map(location = c(left=-79.6, bottom=43.58, right=-79.18, top=43.85), 
                   source = "google", maptype = "roadmap", color='bw')

#Graph map!
map_plot = ggmap(tor_box) + 
  geom_point(data = shelters_by_avg_cap, 
             mapping = aes(x = lon...16, y = lat...17, 
                           size = avg_occ, color=sector)) +
  scale_color_manual(values = color_scheme2) +
  guides(colour = guide_legend(override.aes = list(size=10)))

