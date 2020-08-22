
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)

#Import Data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

#Function to return the row/game that represents the end of the longest streak, return the length of streak and team
get_longest_streak <- function(df) {
  streak <- max(df$goal_streak, na.rm = TRUE)
  row <- df %>% filter(df$goal_streak == streak)
  team <- row$team
  return(c(streak, team))
}

#Create a list of all players in the individual games df
available_players <- unique(game_goals$player)

#Get a list of the top 11 players (11 since 3 of the top overall scorers are not in the game_goals database)
all_top_players <- head(top_250,11)

#Only keep the top 8 by finding the overlap of the top11 list and the available players
top_players <- intersect(all_top_players$player,available_players)

#Create an empty df to add data to in each loop
all_streaks_df = data.frame()

#Loop through all players in the list
for (player_loop in top_players) {
  
  #Filter the game df down to just the player
  player_df <- game_goals %>% filter(game_goals$player == player_loop)
  
  #Create a list of all seasons played by the player for looping and plotting
  player_seasons <- c(unique(player_df$season))

  #Create empty lists to add values to for plotting
  goal_streak_list = c()
  no_goal_streak_list = c()
  team_list_goal = c()
  team_list_no_goal = c()
  
  #Loop through all seasons played by player in loop
  for (season_loop in player_seasons) {
    
    #Filter down to just the season and player
    player_df_year <- player_df %>% filter(player_df$season == season_loop)
    
    #Add a column that just indicates if a goal was scored in the game or not
    player_df_year <- player_df_year %>% mutate(goal_game = case_when(goals == 0 ~ 0,
                                                        goals > 0 ~ 1))
  
    #Create a sequence of running totals for if a goal was scored or not
    goal_streak <- sequence(rle(as.character(player_df_year$goal_game))$lengths)
  
    #Slap the sequence above onto the end of the player/year df
    player_df_year <- cbind(player_df_year, goal_streak)
    
    #Split the df into one that shows all games with goals and one with all games with no goals
    player_df_goals <- player_df_year %>% filter(player_df_year$goal_game == 1)
    player_df_no_goals <- player_df_year %>% filter(player_df_year$goal_game == 0)
  
    #Use user-created function above to get the longest streak (and drought) and the team when that streak happened
    longest_goal_streak <- get_longest_streak(player_df_goals)
    longest_no_goal_streak <- get_longest_streak(player_df_no_goals)
  

    #Slap all the results onto the list
    goal_streak_list <- c(goal_streak_list, longest_goal_streak[1])
    no_goal_streak_list <- c(no_goal_streak_list, longest_no_goal_streak[1])
    team_list_goal <- c(team_list_goal, longest_goal_streak[2])
    team_list_no_goal <- c(team_list_no_goal, longest_no_goal_streak[2])
  }

  #Reset streak lists as integers because they got stored as a str in the multiple output function
  goal_streak_list <- as.integer(unlist(strsplit(goal_streak_list,",")))
  no_goal_streak_list <- as.integer(unlist(strsplit(no_goal_streak_list,",")))
  
  #Get the length of the lists to indicate grouping in the df
  career_length <- length(player_seasons)
  
  #Combine the two types of streaks together for plotting
  streaks = c(goal_streak_list, no_goal_streak_list)
  teams = c(team_list_goal, team_list_no_goal)
  
  #Add everything together to make a df of all the player's streaks for all seasons
  streak_df <- data.frame(player_seasons, streaks, teams, type=rep(c('Goal Streak', "Goals Drought"), each = career_length))
  
  #Slap on the player name 
  streak_df <- streak_df %>% mutate(player = player_loop)
  
  #Add player's df to the full df of all players
  all_streaks_df <- rbind(all_streaks_df, streak_df)
  
}

#Drop NAs
all_streaks_df <- drop_na(all_streaks_df)

#Reset order of players to have totals show in order
all_streaks_df$player <- factor(all_streaks_df$player, levels = top_players)

#Hard code colors for each team, teams show up alphabetical in graph
colors_final <- c('#C8102E', '#006847', '#CE1126', '#FF4C00', '#C8102E', '#A2AAAD', '#009639', '#CE1126', '#0038A8', '#F74902', '#8C2633', '#FCB514', '#002F87', '#00205B', '#00843D', '#C8102E')

#Plot
ggplot(all_streaks_df, aes(alpha=type, fill=teams, y=streaks, x=player_seasons)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(title = 'Longest Streak of Games with(out) Goals', y='Number of Games in Streak', x='Season', caption='Streaks greater than 10 are cut off to better zoom in on the bulk of the data.') + 
  #Limit the y-axis to focus in on just 0-10, add caption (above) to explain
  coord_cartesian( ylim = c(0,10)) +
  scale_fill_manual(values=colors_final) + 
  scale_color_manual(values=colors_final) + 
  scale_alpha_manual(values = c(1,0.3)) + 
  scale_y_continuous(breaks=seq(0,10,5)) +
  theme(
    panel.spacing = unit(0.9, "lines"),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
  facet_wrap(~player, ncol = 2, scales='free_x') 
  

