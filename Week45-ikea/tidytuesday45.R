library(dplyr)
library(tidytuesdayR)
library(ggplot2)
library(tidyr)
library(tidyverse)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

#Drop all furniture that is missing a dimension and thus doesn't have a volume
ikea = ikea %>% drop_na()

#Create volume column and convert saudi riyals to USD
ikea$volume = (ikea$depth * ikea$height * ikea$width)/1000000
ikea$USD = ikea$price * 0.26635

#List of categories without much data to make facet focus on most important cats
smallest_cats = c("Bar furniture", "Café furniture", "Room dividers", "Sideboards, buffets & console tables", "Trolleys")

#Split df for graphing
ikea_main = ikea %>% filter(!(category %in% smallest_cats))
ikea_small = ikea %>% filter((category %in% smallest_cats))

#Plot
ggplot(ikea_main, aes(x=USD, y=volume)) +
  geom_point() + facet_wrap(~category, scales="free") +
  ylab('Volume in m^3')
