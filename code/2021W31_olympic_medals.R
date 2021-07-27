# Tidy Tuesday: 2021 Week 31
# 27 July 2021: Olympic Medals
# This codes creates a line graph of the average weight and height of medal winners in the Olympics
library(dplyr)
library(ggplot2)
library(extrafont)
library(imager)
library(cowplot)

# Load olympic rings logo
olympics_logo <- load.image(file = "./images/olympic_rings.png")

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympics_weights_plot =
  olympics %>% filter(season=='Summer' & !is.na(medal) & !is.na(weight) &
  sport %in% c('Athletics', 'Swimming', 'Sailing', 'Gymnastics',
               'Fencing', 'Weightlifting', 'Wrestling', 'Boxing',
               'Canoeing', 'Diving', 'Rowing', 'Cycling') ) %>% 
  group_by(year, sex, sport, name) %>%
  summarise(min_weight=min(weight)) %>%
  group_by(year, sex, sport) %>%
  summarise(mean_weight = mean(min_weight)) %>%
  ggplot(aes(x=year, y=mean_weight, col=sex)) +
  geom_line(size=1) +
  facet_wrap(facets=vars(sport), ncol=4) +
  labs(title = 'Average weight of Summer Olympic medal winners over time',
       y='Average Weight (kg)', x='Year',
       col='Gender',
       caption='Data Source: Kaggle\n Created by: dosullivan019') +
  scale_colour_manual(labels = c('Female', 'Male'),
                      values = c('sienna1', 'cornflowerblue')) +
  theme_minimal() +
  theme(legend.position='top',
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        plot.title = element_text(hjust=0.5, family='Georgia', size=18),
        panel.grid.minor = element_blank(),
        panel.grid.major=element_line(colour='grey95', linetype=2),
        strip.text = element_text(family='Georgia', size=13),
        plot.caption = element_text(face='italic', size=8))

ggdraw(olympics_weights_plot) + 
  draw_image(olympics_logo, x=-0.385, y=0.475, scale=0.1) +
  draw_image(olympics_logo, x=0.405, y=0.475, scale=0.1) +
  theme(plot.margin = unit(c(1,1,1,1), 'cm'))


ggsave('plots/2021W31_olympic_medals.png', width=12, height=10.5)
