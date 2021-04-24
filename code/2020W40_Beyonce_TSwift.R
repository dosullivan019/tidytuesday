# Tidy Tuesday 30 Sept 2020
# Beyonce and Taylor Swift Lyrics
# Sentiment Analysis of both artists
library(ggplot2)
library(dplyr)
library(tidytext)   # for text mining
library(extrafont)

# TODO: remove duplicates from live/club version/albums

# Read in data
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

# tokenize words from lyrics
tay_words <- taylor_swift_lyrics %>% 
  unnest_tokens(word, Lyrics) %>% select(artist=Artist,
                                         song_title=Title,
                                         word=word)

beyonce_words <- beyonce_lyrics %>% 
  unnest_tokens(word, line) %>% select(artist=artist_name,
                                         song_title=song_name,
                                         word=word)

# calculate sentiment emotions of both artists using nrc lexicon
emotions <- rbind(beyonce_words, tay_words) %>%
  inner_join(get_sentiments("nrc"), by='word') %>%
  group_by(artist) %>% filter(!(sentiment %in% c('positive', 'negative'))) %>%
  mutate(no_songs=length(unique(song_title))) %>% 
  group_by(artist, no_songs) %>% count(sentiment) %>% 
  # calculate sentiment as a proportion of songs to normalise
  mutate(sentiment_prop=ifelse(artist=='Taylor Swift',n,-n)/no_songs) 

# plot of emotions 
emotions %>%
  ggplot(aes(x=reorder(sentiment,abs(sentiment_prop)), 
             y=sentiment_prop,
             fill=artist)) + 
  geom_bar(stat='identity') + coord_flip() + ylab('Word Count / Number of Songs by artist') +
  scale_y_continuous(breaks = pretty(emotions$sentiment_prop),
                     labels = abs(pretty(emotions$sentiment_prop)), limits=c(-16,16)) +
  labs(title='Sentiment Text Analysis',
       subtitle = "Emotions of Beyonce and Taylor Swift lyrics",
       fill = "Artist") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x= element_text(colour='white',size=10,family="Ink Free"),
        axis.text.y = element_text(colour='white',size=18,family="Ink Free"),
        axis.text.x = element_text(colour='white'),
        legend.position = 'top',
        plot.title=element_text(hjust=0.5, colour='white',size=25,family="Ink Free"),
        plot.subtitle=element_text(hjust=0.5, colour='white',size=18,family="Ink Free"),
        plot.background = element_rect(fill='dimgrey',color=NA),
        legend.title=element_text(colour='white', size=13, family="Ink Free"),
        legend.text = element_text(colour='white', size=13, family="Ink Free"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank()
        )
ggsave('./plots/beyonce_tswift.png')
