library(dplyr)
library(tidyr)
library(stringr)
library(ggraph)
library(igraph)
library(extrafont)
library(tibble)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
netflix_titles$cast_and_director = paste(netflix_titles$director, ', ', netflix_titles$cast, sep='') # combine director into cast

list_of_cast = 
  netflix_titles %>% filter(grepl('Leonardo DiCaprio', cast)) %>%           # Filter for only title Leo appears in
  mutate(cast_director_list=stringr::str_split(cast_and_director, fixed(", ")))  # split into individuals
                          
all_pairs = lapply(list_of_cast$cast_director_list, combn, 2, simplify = FALSE)   # Find combinations of all actors and directors

network_data = as.data.frame(matrix(unlist(all_pairs), ncol = 2, byrow = TRUE))
colnames(network_data) = c('person_1', 'person_2')

graph_data = graph_from_data_frame(network_data, directed=FALSE)

ggraph(graph_data) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_label(aes(label=name)) +
  labs(title='Leonardo DiCaprio Connections on Netflix',
       caption='Tidy Tuesday | Data Source: Kaggle & Flixable\n Created by: dosullivan019') +
  theme(legend.position = 'none',
        plot.title=element_text(family='Goudy Stout', hjust=0.5))
ggsave('plots/2021W17_netflix.png', width=15, height=10)


