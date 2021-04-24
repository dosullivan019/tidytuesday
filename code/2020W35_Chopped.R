# Tidy Tuesday 25 Aug 2020
# Chopped TV Show
library(ggplot2)
library(dplyr)
library(tidytext) #  for text mining
library(wordcloud2)
library(wesanderson)
library(webshot)
# webshot::install_phantomjs()
library("htmlwidgets")
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

# count NA's
# sapply(chopped, function(x) sum(is.na(x)))

# find each individual word used in entree and count how many time it was in the show
entrees_ingredients <- chopped %>% select(entree) %>%
  unnest_tokens(word, entree) %>% count(word, sort=TRUE)

# remove any stop words using the stopwords dataset
cleaned_entree <- entrees_ingredients %>%
  anti_join(get_stopwords())

# Create wordcloud plot
word_plot <- wordcloud2(cleaned_entree, shape='circle', size=0.75, 
           color=rep(wes_palettes$Rushmore1[3:5], nrow(cleaned_entree)/length(wes_palettes$Rushmore1[3:5])))

# save widget in html
saveWidget(word_plot,"plots/word_plot.html",selfcontained = F)

# and in png
webshot("word_plot.html","plots/chopped_word_plot.png", delay =5)
