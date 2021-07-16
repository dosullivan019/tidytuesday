# Tidy Tuesday: 2021 Week 28
# 6 July 2021: International Independence Days
# This creates a map of the world showing the years when countries gained independence from UK

library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(viridis)
library(extrafont)

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

# Creating map using rnaturalearth
rne_map <- ne_countries(scale = "medium", returnclass = "sf")
rne_map$country = rne_map$name

rne_map %>%
  left_join(holidays, by='country') %>% 
  mutate(freedom = ifelse(!is.na(independence_from), 1, 0),
         free_from_uk_decade = ifelse(independence_from == 'United Kingdom' | 
           independence_from == 'United Kingdom of Great Britain and Ireland' |
           independence_from == 'Kingdom of Great Britain', year, NA),
         bin = cut_width(free_from_uk_decade, width=10, center=1960)) %>% 
  ggplot(aes(fill = bin)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = 'lightgrey', discrete=TRUE, direction=-1) + 
  labs(title = 'Gaining independence from the UK',
       fill = 'Year',
       caption='Data Source: Wikipedia\n Created by: dosullivan019') +
  geom_text(x=-30, y=52.5, label='Ireland became \nindependent in 1916', size=2.5, family="Lucida Calligraphy") +
  geom_text(x=-150, y=45, label='USA were the first country\n to gain independence \nfrom the UK in 1776', size=2.5, family="Lucida Calligraphy") +
  theme_void() +
  theme(legend.position = 'top',
        plot.title = element_text(hjust=0.5, family="Lucida Calligraphy"),
        legend.title = element_text(family="Lucida Calligraphy"),
        plot.caption = element_text(colour='grey30', face = 'italic',size=7)) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)))
  
ggsave('plots/2021W28_independence.png')


