

library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggwordcloud)

setTimeLimit(elapse=4)

#Import data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

get_wordcloud_graph <- function(word, list) {
  word_list = grep(word, list, value=TRUE)
  
  word_list = strsplit(word_list, ' ')
  word_list = unlist(word_list, recursive = FALSE)
  word_list = removeWords(word_list,word)
  
  word_table = table(word_list)
  word_df = as.data.frame.table(word_table)
  
  word_df = word_df %>% filter(word_df$Freq > 3)
  
  word_cloud_graph = ggplot(word_df, aes(label = word_list, size=Freq)) +
    geom_text_wordcloud(area_corr_power = 1, rm_outside = TRUE, family="Purisa") +
    theme(panel.background = element_rect(fill = 'white')) +
    theme(plot.margin = margin(l=5,r=5,t=5,b=5)) +
    scale_size_area(max_size = 14)
  return(word_cloud_graph)
}

#Get ingredient lists by course
app_ingredients = chopped$appetizer
entree_ingredients = chopped$entree
dessert_ingredients = chopped$dessert

#Split ingredient lists into separate ingredients
app_ingredients_split = strsplit(app_ingredients, ', ')
entree_ingredients_split = strsplit(entree_ingredients, ', ')
dessert_ingredients_split = strsplit(dessert_ingredients, ', ')


#Merge list of list of ingredients into one giant list by course
app_ingredients_split = unlist(app_ingredients_split, recursive = FALSE)
entree_ingredients_split = unlist(entree_ingredients_split, recursive = FALSE)
dessert_ingredients_split = unlist(dessert_ingredients_split, recursive = FALSE)

#Split ingredients lists into single words (ingredient types)
app_ingredient_types = strsplit(app_ingredients_split, ' ')
entree_ingredient_types = strsplit(entree_ingredients_split, ' ')
dessert_ingredient_types = strsplit(dessert_ingredients_split, ' ')


#Merge list of list of ingredients into one giant list by course
app_ingredient_types = unlist(app_ingredient_types, recursive = FALSE)
entree_ingredient_types = unlist(entree_ingredient_types, recursive = FALSE)
dessert_ingredient_types = unlist(dessert_ingredient_types, recursive = FALSE)

#Get list of ingredients that have been used in all three courses
all_courses_ingredients = Reduce(intersect, list(app_ingredients_split, entree_ingredients_split, dessert_ingredients_split))

#Create a frequency table of appetizer ingredients
app_ingredients_table = table(app_ingredients_split)
app_ingredients_df = as.data.frame.table(app_ingredients_table)

#Create a frequency table of appetizer ingredient types
app_ingredient_types_table = table(app_ingredient_types)
app_ingredient_types_df = as.data.frame.table(app_ingredient_types_table)
app_ingredient_types_df = app_ingredient_types_df %>% mutate(course = 'Appetizer')

#Filter top 10 app ingredients
app_ingredients_df = app_ingredients_df[order(-app_ingredients_df$Freq),]
app_ingredients_df = app_ingredients_df %>% mutate(course = 'Appetizer')


#Create a frequency table of entree ingredients
entree_ingredients_table = table(entree_ingredients_split)
entree_ingredients_df = as.data.frame.table(entree_ingredients_table)

#Create a frequency table of entree ingredient types
entree_ingredient_types_table = table(entree_ingredient_types)
entree_ingredient_types_df = as.data.frame.table(entree_ingredient_types_table)
entree_ingredient_types_df = entree_ingredient_types_df %>% mutate(course = 'Entree')

#Get top 10 entree ingredients
entree_ingredients_df = entree_ingredients_df[order(-entree_ingredients_df$Freq),]
entree_ingredients_df = entree_ingredients_df %>% mutate(course = 'Entree')

#Get a frequency table of dessert ingredients
dessert_ingredients_table = table(dessert_ingredients_split)
dessert_ingredients_df = as.data.frame.table(dessert_ingredients_table)

#Create a frequency table of dessert ingredient types
dessert_ingredient_types_table = table(dessert_ingredient_types)
dessert_ingredient_types_df = as.data.frame.table(dessert_ingredient_types_table)
dessert_ingredient_types_df = dessert_ingredient_types_df %>% mutate(course = 'Dessert')

#Get top 10 dessert ingredients
dessert_ingredients_df = dessert_ingredients_df[order(-dessert_ingredients_df$Freq),]
dessert_ingredients_df = dessert_ingredients_df %>% mutate(course = 'Dessert')

#Create a list of all ingrediets ever assigned
all_ingredients = c(app_ingredients_split, entree_ingredients_split, dessert_ingredients_split)

#Create a freqency table for all ingredients
all_ingredients_table = table(all_ingredients)
all_ingredients_df = as.data.frame.table(all_ingredients_table)

#Get all ingredients that were used more than 10 times
all_ingredients_df = all_ingredients_df[order(-all_ingredients_df$Freq),]
top_ingredients_df = head(all_ingredients_df, 15)

#Get list of ingredients used more than 10 times
top_ingredients = top_ingredients_df$all_ingredients

#Filter each courses df to get just the ingredients that were used more than 10 times overall
top_ingredients_in_apps = app_ingredients_df %>% filter(app_ingredients_df$app_ingredients_split %in% top_ingredients) %>% rename(ingredients=app_ingredients_split)
top_ingredients_in_entrees = entree_ingredients_df %>% filter(entree_ingredients_df$entree_ingredients_split %in% top_ingredients) %>% rename(ingredients=entree_ingredients_split)
top_ingredients_in_desserts = dessert_ingredients_df %>% filter(dessert_ingredients_df$dessert_ingredients_split %in% top_ingredients) %>% rename(ingredients=dessert_ingredients_split)

#Slap together each of the courses top overall ingredients dfs for graphing
top_ingredients_df_graph = rbind(top_ingredients_in_apps, top_ingredients_in_entrees, top_ingredients_in_desserts)


#Re-order the ingredients and courses for graph
top_ingredients_df_graph$ingredients <- factor(top_ingredients_df_graph$ingredients, levels = top_ingredients)
top_ingredients_df_graph$course <- factor(top_ingredients_df_graph$course, levels = c('Dessert', 'Entree','Appetizer' ))

colors_graphs = c('#e67f83', '#ffd7ac', '#ff6a00')
#OVerall ingredients graph
overall_ingredients_graph = ggplot(top_ingredients_df_graph, aes(fill=course, y=Freq, x=ingredients)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=colors_graphs) + 
  scale_color_manual(values=colors_graphs) + 
  theme(legend.position=c(0.9,0.8), legend.title = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank()) +
  theme(plot.background = element_rect(fill = 'white'), panel.background = element_rect(fill = 'transparent'), axis.text = element_text(color = 'black')) +
  theme(plot.margin = margin(l=0,r=0,t=10,b=10)) + 
  theme(legend.background = element_rect(fill = 'white'), legend.text = element_text(color = 'black')) + 
  coord_flip() + 
  ggtitle('Ingredients')

#Split list of all ingredients into individual words
all_ingredients_split = strsplit(all_ingredients, ' ')
all_ingredients_split = unlist(all_ingredients_split, recursive = FALSE)

#Create a freqency table of all ingredient words
all_ingredients_split_table = table(all_ingredients_split)
all_ingredients_split_df = as.data.frame.table(all_ingredients_split_table)

#Get ingredients words used 60 times or more
all_ingredients_split_df = all_ingredients_split_df[order(-all_ingredients_split_df$Freq),]
top_all_ingredients_split_df = head(all_ingredients_split_df, 15)
top_all_ingredients_split = top_all_ingredients_split_df$all_ingredients_split

#Filter each courses df to get just the ingredient words that were used more than 60 times overall
top_ingredient_types_apps = app_ingredient_types_df %>% filter(app_ingredient_types_df$app_ingredient_types %in% top_all_ingredients_split) %>% rename(ingredients=app_ingredient_types)
top_ingredient_types_entrees = entree_ingredient_types_df %>% filter(entree_ingredient_types_df$entree_ingredient_types %in% top_all_ingredients_split) %>% rename(ingredients=entree_ingredient_types)
top_ingredient_types_desserts = dessert_ingredient_types_df %>% filter(dessert_ingredient_types_df$dessert_ingredient_types %in% top_all_ingredients_split) %>% rename(ingredients=dessert_ingredient_types)

#Slap together each of the courses top overall ingredient word dfs for graphing
top_all_ingredients_split_df_graph = rbind(top_ingredient_types_apps, top_ingredient_types_entrees, top_ingredient_types_desserts)


#Re-order the ingredient words and courses for graph
top_all_ingredients_split_df_graph$ingredients <- factor(top_all_ingredients_split_df_graph$ingredients, levels = top_all_ingredients_split)
top_all_ingredients_split_df_graph$course <- factor(top_all_ingredients_split_df_graph$course, levels = c('Dessert', 'Entree','Appetizer' ))

#OVerall ingredient type graph
ingredients_type_graph <- ggplot(top_all_ingredients_split_df_graph, aes(fill=course, y=Freq, x=ingredients)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.title.y = element_blank(), legend.position='none', axis.title.x = element_blank()) + 
  theme(plot.background = element_rect(fill = 'white'), panel.background = element_rect(fill = 'transparent'), axis.text = element_text(color = 'black')) +
  theme(plot.margin = margin(l=0,r=0,t=10,b=10)) + 
  scale_fill_manual(values=colors_graphs) + 
  scale_color_manual(values=colors_graphs) + 
  coord_flip() +
  ggtitle('Ingredient Types')


#Get word clouds for top six ingredient types
chocolate_graph <- get_wordcloud_graph('chocolate',all_ingredients)
cream_graph <- get_wordcloud_graph('cream',all_ingredients)
chicken_graph <- get_wordcloud_graph('chicken',all_ingredients)
pork_graph <- get_wordcloud_graph('pork',all_ingredients)
cheese_graph <- get_wordcloud_graph('cheese',all_ingredients)
baby_graph <- get_wordcloud_graph('baby',all_ingredients)


overall_ingredients_graph/(ingredients_type_graph | (cheese_graph | chocolate_graph | cream_graph)/(chicken_graph | pork_graph | baby_graph)) +
  plot_layout(heights = c(1,1.5)) +
  plot_annotation(theme = theme(panel.background = element_rect(fill = 'white'))) +
  plot_annotation(
    title = "Chopped: Most Popular Ingredients and Types",
    subtitle = "Word Clouds are top six ingredient types",
    caption = "Created by @jesseroe55, Data from #TidyTuesday.")



c1_info = chopped$contestant1_info
c2_info = chopped$contestant2_info
c3_info = chopped$contestant3_info
c4_info = chopped$contestant4_info

c1_info_split = strsplit(c1_info, ' ')
c1_info_split = unlist(c1_info_split, recursive = FALSE)
c2_info_split = strsplit(c2_info, ' ')
c2_info_split = unlist(c2_info_split, recursive = FALSE)
c3_info_split = strsplit(c3_info, ' ')
c3_info_split = unlist(c3_info_split, recursive = FALSE)
c4_info_split = strsplit(c4_info, ' ')
c4_info_split = unlist(c4_info_split, recursive = FALSE)

contestant_info = c(c1_info_split, c2_info_split, c3_info_split, c4_info_split)

contestant_info = removeWords(contestant_info," ")

contestant_info_table = table(contestant_info)
contestant_info_df = as.data.frame.table(contestant_info_table)


contestant_info_df = contestant_info_df[order(-contestant_info_df$Freq),]
dropwords = c('','from','and','&','-','Episode','de', 'Charity:', '-')

contestant_info_df = contestant_info_df %>% filter(!contestant_info_df$contestant_info %in% dropwords)
contestant_info_df = head(contestant_info_df,14)

contestant_graph = ggplot(contestant_info_df, aes(x=Freq, y= reorder(contestant_info, -Freq))) + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) + 
  theme(plot.background = element_rect(fill = 'white'), panel.background = element_rect(fill = 'transparent'), axis.text = element_text(color = 'black')) +
  theme(plot.margin = margin(l=0,r=,t=0,b=0)) + 
  geom_bar(stat="identity") +
  ggtitle('Contestant Details')

