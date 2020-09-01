
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')

get_graph <- function(list, type) {
  top_df = fertilizer_full %>% filter(fertilizer_full$Entity %in% list)
  names(top_df)[4] <- "Cereal"
  names(top_df)[5] <- "Fertilizer"
  
  top_df = top_df %>% mutate(grouping = type)
  return(top_df)
}

fertilizer_full = fertilizer %>% drop_na()

fertilizer_full = fertilizer_full[order(-fertilizer_full$`Cereal yield (tonnes per hectare)`),]
top_yields = head(fertilizer_full,25)
top_yield_countries = unique(top_yields$Entity)


fertilizer_full = fertilizer_full[order(-fertilizer_full$`Nitrogen fertilizer use (kilograms per hectare)`),]
top_ferts = head(fertilizer_full,25)
top_fert_countries = unique(top_ferts$Entity)

top_pop_countries = c('China', 'India', 'United States', 'Indonesia', 'Pakistan', 'Brazil')
top_wheat_countries = c('China', 'India', 'United States', 'Russia', 'France', 'Canada')
top_rice_countries = c('China', 'India', 'Indonesia', 'Bangladesh', 'Vietnam', 'Thailand')
top_maize_countries = c('United States', 'China', 'Brazil', 'Argentina', 'Mexico', 'India')



top_yield_df = get_graph(top_yield_countries, 'Top Yield Countries')
top_yield_df
top_fert_df = get_graph(top_fert_countries, 'Top Fertilzer Users')
top_pop_df = get_graph(top_pop_countries, 'Top Population Countries')

top_maize_df = get_graph(top_maize_countries, 'Top Maize Producers')
top_rice_df = get_graph(top_rice_countries, 'Top Rice Producers')
top_wheat_df = get_graph(top_wheat_countries, 'Top Wheat Producers')

graph_df_1 = rbind(top_yield_df, top_fert_df, top_pop_df)
graph_df_2 = rbind(top_maize_df, top_rice_df, top_wheat_df)

ggplot(graph_df_1, aes(x=Year, y=Cereal, color=Entity)) + 
  geom_line(aes(size=Fertilizer)) + 
  scale_size('Fertilizer Use (kg/hectare)', range = c(0, 8)) + 
  facet_wrap(~ grouping) + 
  labs(y='Cereal Yield (tonnes/hectare') +
  theme(plot.background = element_rect(fill = 'white'), panel.background = element_rect(fill = 'transparent'), axis.text = element_text(color = 'black'),
  panel.grid.major.x = element_line( size=.1, color="gray"),
  panel.grid.major.y = element_line( size=.1, color="gray")) + 
  guides(colour = guide_legend(override.aes = list(size=5)))
