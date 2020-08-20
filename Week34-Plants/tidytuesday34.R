library(ggplot2)
library(RColorBrewer)

# Import Libraries
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

glimpse(plants)
# Drop the values where the year last scene of the plant is not known
actions <- filter(actions, year_last_seen != 'NA')
threats <- filter(threats, year_last_seen != 'NA')

# Group by continent, then year last seen, then action_type to create percentages over time
actions_over_time <- actions %>%
  group_by(continent, year_last_seen, action_type) %>%
  summarize(action_sum=sum(action_taken))

threats_over_time <- threats %>%
  group_by(continent, year_last_seen, threat_type) %>%
  summarize(threat_sum=sum(threatened))

# Reorder to get Before 1900 to show up first
actions_over_time$year_last_seen <- factor(actions_over_time$year_last_seen, levels = c('Before 1900', '1900-1919','1920-1939', '1940-1959', '1960-1979', '1980-1999', '2000-2020'))
threats_over_time$year_last_seen <- factor(threats_over_time$year_last_seen, levels = c('Before 1900', '1900-1919','1920-1939', '1940-1959', '1960-1979', '1980-1999', '2000-2020'))

#graph of all actions
actions_all <- ggplot(actions_over_time, aes(fill=action_type, y=action_sum, x=year_last_seen)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Change in Action Type over Time', x='Year Last Seen', y='Percentage of Total Actions') + 
  scale_fill_brewer(palette='Spectral')

#graph of actions by continent
actions_cont <- ggplot(actions_over_time, aes(fill=action_type, y=action_sum, x=year_last_seen)) + 
  geom_bar(position="fill", stat="identity") + 
  facet_wrap(~continent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Change in Action Type over Time', x='Year Last Seen', y='Percentage of Total Actions') +
  scale_fill_brewer(palette='Spectral')

#graph of all threats
threats_all <- ggplot(threats_over_time, aes(fill=threat_type, y=threat_sum, x=year_last_seen)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Change in Threat Type over Time', x='Year Last Seen', y='Percentage of Total Threats') +
  scale_fill_brewer(palette='RdYlBu')

#graph of threats by continent
threats_cont <- ggplot(threats_over_time, aes(fill=threat_type, y=threat_sum, x=year_last_seen)) + 
  geom_bar(position="fill", stat="identity") + 
  facet_wrap(~continent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Change in Threat Type over Time', x='Year Last Seen', y='Percentage of Total Threats') + 
  scale_fill_brewer(palette='RdYlBu')


actions_all
