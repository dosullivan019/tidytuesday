# Tuesday 08 Sept 2020
# Friends
library(ggplot2)
library(dplyr)
library(tidytext)   # for text mining
library(extrafont)  # to import new fonts

# How to load a new font
# font_import(paths=c("C:/PATH_TO_NEW_FONT"), prompt = FALSE)
# loadfonts(device = "win")
# fonts_tb = windowsFonts()

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

friends_characters= c('Monica Geller',
                      'Joey Tribbiani',
                      'Chandler Bing',
                      'Ross Geller',
                      'Rachel Green',
                      'Phoebe Buffay')


# Use ngrams to find the most popular sequence of 3 words & count frequency
friends_trigrams <- friends %>% select(speaker, text) %>% filter(speaker %in% friends_characters) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  count(speaker, trigram, sort=TRUE)

# Get top 3 phrases by speaker
catchphrases_top3 = friends_trigrams %>% group_by(speaker) %>% 
                      slice_max(order_by = n, n = 3) %>% 
                      mutate(n_phrase=row_number(), n_friend=cur_group_id())

# Define x and y co-ordinates
catchphrases_top3['x_coord'] <- ifelse(catchphrases_top3$n_friend < 4, 
                                       catchphrases_top3$n_friend,
                                       catchphrases_top3$n_friend-3)
catchphrases_top3['y_coord'] <- ifelse(catchphrases_top3$n_friend < 4,1,2)

# Define colours
friend_cols = c(rep('dodgerblue3',3), rep('purple',3), rep('yellowgreen',3),
                rep('firebrick2',3), rep('yellow2',3), rep('forestgreen',3))

# Plot
ggplot(catchphrases_top3, aes(x=speaker)) + ylim(25,230) + xlim(30,170) +
  # speaker
  geom_text(aes(x=x_coord*50, y=y_coord*100, label=speaker), colour=friend_cols, family="Gabriel Weiss' Friends Font", size=10) +
  # catchphrase
  geom_text(aes(x=x_coord*50, y=y_coord*100-n_phrase*20, label=trigram), colour='white', 
            family="Gabriel Weiss' Friends Font", size=8) +
  theme_minimal() +
  theme(plot.title=element_text(size=40,family="Gabriel Weiss' Friends Font", hjust = 0.5, vjust=-3, colour = 'white'),
        plot.subtitle=element_text(size=25,family="Gabriel Weiss' Friends Font", hjust = 0.5, vjust=-15, colour='white'),
        plot.caption =element_text(size=10, hjust=1, face='italic', color="white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill='black'),
        axis.line=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  labs(title=bquote("F  R  I  E  N  D  S"),
       subtitle = 'The one with the catchphrases',
       caption = 'Data Source: friends data package')

ggsave('friends.png', path='plots', width=16, height=10)

