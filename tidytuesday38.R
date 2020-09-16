
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(tidytext)
library(gghighlight)
library(data.table)
library(forcats)
library(gganimate)
library(gifski)
library(av)

#Import Data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

#Set up regions to make graph easier to read
NE = c('Maine', 'Vermont', 'New Hampshire', 'Massachusetts', 'New York', 'New Jersey', 'Rhode Island', 'Pennsylvania', 'Delaware', 'Maryland', "Connecticut")
SE = c('District of Columbia', 'Virginia', 'West Virginia', 'North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Alabama', 'Mississippi', 'Louisiana')
MW = c('Kentucky', 'Ohio', 'Michigan', 'Indiana', 'Illinois', 'Wisconsin', 'Minnesota', 'Missouri', 'Arkansas', 'Tennessee')
CT = c('Texas', 'Oklahoma', 'Kansas', 'South Dakota', 'North Dakota', 'Wyoming', 'Colorado', 'Iowa', 'New Mexico', 'Nebraska')
WS = c('Arizona', 'Nevada', 'Utah', 'Idaho', 'California', 'Oregon', 'Montana', 'Washington', 'Alaska', 'Hawaii')

#Combine regions into a df for join with main data
state_region_ne = data.frame(NE)
state_region_ne$region = 'Northeast'
state_region_se = data.frame(SE)
state_region_se$region = 'Southeast'
names(state_region_se) = names(state_region_ne)
state_region_mw = data.frame(MW)
state_region_mw$region = 'Midwest'
names(state_region_mw) = names(state_region_ne)
state_region_ct = data.frame(CT)
state_region_ct$region = 'Central'
names(state_region_ct) = names(state_region_ne)
state_region_ws = data.frame(WS)
state_region_ws$region = 'West'
names(state_region_ws) = names(state_region_ne)

state_region_df = rbind(state_region_ws,state_region_ct,state_region_mw, state_region_ne, state_region_se)
names(state_region_df) = c('State', 'Region')

#List of education spending categories
ed_cats = c('PK12ed', 'highered', 'edsubs', 'edservs', 'pell', 'HeadStartPriv')
#Get totals spending per child and overall for each state and year
kid_totals = kids %>% group_by(kids$state, kids$year) %>% summarise(total = sum(inf_adj_perchild), grand_total = sum(inf_adj)/1000000)
#Get total ed spending per state per year
kid_eds = kids %>%  group_by(kids$state, kids$year) %>% subset(kids$variable %in% ed_cats) %>% summarise(eds = sum(inf_adj_perchild))

#Combine dfs
kid_final = merge(x= kid_totals, y = kid_eds, by=c('kids$state','kids$year'))
#Change column names for easier graphing
names(kid_final) = c('State', 'Year', "Total", 'TotalSpending', "Education")
#Join regions and drop NAs
kid_final = left_join(kid_final, state_region_df)
kid_final = drop_na(kid_final)

#Filter dfs to regions
kid_final_ne = kid_final %>% filter(kid_final$Region == 'Northeast')
kid_final_se = kid_final %>% filter(kid_final$Region == 'Southeast')
kid_final_mw = kid_final %>% filter(kid_final$Region == 'Midwest')
kid_final_ct = kid_final %>% filter(kid_final$Region == 'Central')
kid_final_ws = kid_final %>% filter(kid_final$Region == 'West')

#Graph
plot = ggplot(kid_final_mw, aes(x=Total, y=Education, color=State, fill=State)) + geom_point(aes(size = TotalSpending)) + 
  geom_abline(intercept = 0, slope = 0.5, color="red", 
  linetype="dashed", size=1.5) + 
  annotate("text", x = 10, y = 0, label = "Red Line - 50% of Spending")+ 
  scale_size_continuous(name='Total Spending in Millions', range = c(3, 12)) +
  xlab('Total Overall Expediture Per Child (in thousands)') + 
  ylab('Total Education Expediture Per Child (in thousands)') + 
  guides(color = guide_legend(override.aes = list(size = 8))) + 
  expand_limits(y=0) + expand_limits(x=0) + 
  theme_tufte() +
  theme(panel.grid.major = element_line(color='gray'), text = element_text(size=14))
#Animate
anim = plot + transition_states(kid_final_mw$Year, transition_length = 2, state_length = 1) + 
  ggtitle('Total Education vs Total Overall Spending Per Child in {closest_state}')


# Video output
animate(
  anim + enter_fade(),
  renderer = gifski_renderer())

anim_save("nw_ed_spending.gif")
