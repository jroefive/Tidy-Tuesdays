library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

#Get Data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

#Get total energy use by year for each country
year_totals16 <- aggregate(energy_types$'2016', by=list(energy_types$country), FUN=sum)
year_totals17 <- aggregate(energy_types$'2017', by=list(energy_types$country), FUN=sum)
year_totals18 <- aggregate(energy_types$'2018', by=list(energy_types$country), FUN=sum)

#Change column name to country for join
names(year_totals16)[1] <- "country"
names(year_totals17)[1] <- "country"
names(year_totals18)[1] <- "country"

#Arrange by total use to pick top 5 countries to show on graph
year_totals16 <- year_totals16 %>% arrange(desc(x))
year_totals17 <- year_totals17 %>% arrange(desc(x))
year_totals18 <- year_totals18 %>% arrange(desc(x))

#Slice off just the top 5 for each country
year_totals16 <- head(year_totals16,5)
year_totals17 <- head(year_totals17,5)
year_totals18 <- head(year_totals18,5)

#Create a list of the top 5 countries by energy use for each year
top16 <- year_totals16$country
top17 <- year_totals17$country
top18 <- year_totals18$country

#Combine into one list to get all countries that were in the top 5 at one point
top_countries = unique(c(top16,top17,top18))

#Join the totals to the original df
energy_types_w_totals <- left_join(energy_types, year_totals16, by="country")
energy_types_w_totals <- left_join(energy_types_w_totals, year_totals17, by="country")
energy_types_w_totals <- left_join(energy_types_w_totals, year_totals18, by="country")

#Rename columns
names(energy_types_w_totals)[8] <- "2016 Total"
names(energy_types_w_totals)[9] <- "2017 Total"
names(energy_types_w_totals)[10] <- "2018 Total"

#Create columns with the percent of total usage for each energy type
energy_types_w_totals_percs <- energy_types_w_totals %>% 
  mutate('16' = energy_types_w_totals$'2016'/energy_types_w_totals$'2016 Total') %>% 
  mutate('17' = energy_types_w_totals$'2017'/energy_types_w_totals$'2017 Total') %>% 
  mutate('18' = energy_types_w_totals$'2018'/energy_types_w_totals$'2018 Total')

#Drop down only to the data needed for the graph 
energy_types_w_totals_percs <- energy_types_w_totals_percs %>% select('country','country_name','type','16','17','18')

#Keep only the top 6 countries
energy_types_w_totals_percs <- subset(energy_types_w_totals_percs, energy_types_w_totals_percs$country %in% top_countries)

#Keep only the renewable sources
energy_types_w_totals_percs <- subset(energy_types_w_totals_percs, energy_types_w_totals_percs$type %in% c('Wind','Solar','Hydro', 'Pumped hydro power', 'Geothermal','Other'))

#Pivot long for graphing
energy_types_w_totals_percs <- energy_types_w_totals_percs %>% pivot_longer(-c('country','country_name', 'type'),names_to = 'Year', values_to= "Percentage of Total Usage in Year")

#Reorder types to show better on the graph
energy_types_w_totals_percs$type <- factor(energy_types_w_totals_percs$type, levels = c("Hydro", "Wind", "Solar", "Geothermal", "Pumped hydro power", "Other"))

#Plot all types wrapped
combined <- energy_types_w_totals_percs %>%
  ggplot(aes(x = Year, y = `Percentage of Total Usage in Year`, color = country, group=1)) +
  geom_freqpoly(stat = "identity", size = 1) +
  facet_wrap(vars(type)) + 
  labs(title = "Percent of Total Energay Usage by Renewable Type in Top 6 European Countries") +
  scale_color_solarized() +
  theme_solarized_2() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#Save
path <- "C:/Users/jroef/downloads"
ggsave(path = path, filename = "week_32.png", plot = combined)