# Tidy Tuesday: 2021 Week 16
# 13 April 2021: US Post Offices
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(extrafont)
library(ggpubr)

# Creating map using rnaturalearth
rne_map <- ne_countries(scale = "medium", returnclass = "sf", country='United States of America')

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

map_1820 = 
  ggplot(rne_map) + 
  geom_sf(color = NA) +
  geom_point(data=post_offices[which(post_offices$established <=1820 & 
                                       post_offices$longitude < -60 &
                                       (post_offices$discontinued > 1820 | is.na(post_offices$discontinued))) ,],
             aes(x=longitude, y=latitude),
             colour='navy', size=0.35) +
  xlim(-170, -60) + ylim(15, 75) +
  geom_text(aes(x=-97.5, y=62.5, label='US Post Offices\n1820'), colour='navy', size=9, family='Georgia') +
  theme_void()

map_2020 =
  ggplot(rne_map) + 
  geom_sf(color = NA) +
  geom_point(data=post_offices[which(post_offices$established <=2020 & 
                                       post_offices$longitude < -60 &
                                       (post_offices$discontinued > 2020 | is.na(post_offices$discontinued))),],
               aes(x=longitude, y=latitude),
               colour='navy', size=0.35) +
  xlim(-170, -60) + ylim(15, 75) +
  geom_text(aes(x=-97.5, y=62.5, label='US Post Offices\n2020'), colour='navy', size=9, family='Georgia') +
  theme_void()    
  
ggarrange(map_1820, map_2020, ncol=2,
          labels = list('Data Source: Blevins, Cameron; Helbock, Richard W., 2021, "US Post Offices".', 
                        'Created by: dosullivan019'),
          label.x = c(-0.15, 0.15),
          label.y = c(0.2, 0.17),
          font.label = list(size=10, face='italic'))

ggsave('plots/2021W16_US_Post_Office.png')
