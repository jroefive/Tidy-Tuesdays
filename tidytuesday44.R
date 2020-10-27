
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(ggmap)
library(maps)
library(mapdata)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')
  
#Split df into eastern and western half
west_provs = c('Yukon', 'British Columbia', 'Alberta', 'Northwest Territories', 'Saskatchewan', 'Manitoba')

wind_turbine_west = wind_turbine %>% filter(province_territory %in% west_provs)

wind_turbine_east = wind_turbine %>% filter(!(province_territory %in% west_provs))

#map_bbox <- make_bbox(data = wind_turbine_west, lat = latitude, lon = longitude)
#bc_bbox

#hard coded the bounds to better show the wind turbines
bc_big <- get_map(location = c(left=-94.32712, bottom=40.88028, right=-48.85699, top=50.61859), source = "google", maptype = "hybrid")

ggmap(bc_big) + 
  geom_point(data = wind_turbine_east, mapping = aes(x = longitude, y = latitude, size = turbine_rated_capacity_k_w, color=hub_height_m))