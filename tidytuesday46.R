
library(dplyr)
library(tidytuesdayR)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(gganimate)
library(ggthemes)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

mobile = mobile %>% drop_na()
landline = landline %>% drop_na()

mobile$population_millions = mobile$total_pop/1000000
landline$population_millions = landline$total_pop/1000000

mobile$GDP_per_cap_thousands = mobile$gdp_per_cap/1000
landline$GDP_per_cap_thousands = landline$gdp_per_cap/1000


both = merge(x = mobile, y = landline, by = c("entity","year"), all.x = TRUE)

plot = both %>%
  ggplot(aes(x = landline_subs, y=mobile_subs, size=GDP_per_cap_thousands.x)) +
  geom_point() + 
  facet_wrap(~continent.x) + 
  theme_gdocs() +
  scale_size(range = c(5, 15)) +
  scale_size_binned(n.breaks = 6) +
  theme(legend.position="right", plot.title = element_text(size = 24), strip.text.x = element_text(size = 16)) +
  ylab("Landlines Subscriptions per 100 people") +
  xlab("Mobile Subscriptions per 100 People")


##Animate
anim = plot + transition_states(year, transition_length = 3, state_length = 3) +
  labs(title = 'Landline vs Mobile Use in: {closest_state}')

# Video output
animate(
  anim,
  renderer = gifski_renderer(), height = 800, width =1000)

anim_save("phone_mobile_land.gif")