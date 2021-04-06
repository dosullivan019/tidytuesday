# Tidy Tuesday: 2021 Week 15
# 6 April 2021: Deforestation
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(extrafont)
library(cowplot)
library(magick)

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

soybean_plot <-
  soybean_use %>% 
  filter(entity %in% c('World')) %>% 
  pivot_longer(cols=c(human_food, animal_feed, processed), names_to='description') %>%
  filter(description %in% c('human_food', 'animal_feed')) %>%
  ggplot(aes(x=year, y=value, group=description, col=description)) + geom_line(size=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), limits = c(0,19000000)) +
  scale_x_continuous(limits=c(1960,2020.5), expand=c(0,0)) +
  scale_colour_manual(values=c('cornflowerblue','gold')) +
  geom_text(aes(x=2013, y=18000000, label='Animal Feed'), colour='cornflowerblue',size=4.5, family='Georgia') +
  geom_text(aes(x=2014, y=11050000, label='Human Food'), colour='gold',size=4.5, family='Georgia') +
  labs(title='The Rise of Soybean Production',
       subtitle='Soybean for human consumption has more than doubled since 1960',
       y='',x='Year') + 
  theme(legend.position='none',
        plot.title=element_text(colour ='grey30', size=20, family='Georgia'),
        plot.subtitle=element_text(colour ='grey30', size=15, family='Georgia'),
        panel.background = element_rect(fill= 'white'),
        panel.grid.major=element_line(colour='grey95', linetype=2))
    
soybean_image <- image_read("./images/soybeans.jpg")
ggdraw(soybean_plot) +
  draw_image(soybean_image, x=-0.275, y=0.14, scale=0.5)
ggsave('plots/2021W15_soybean.png')
