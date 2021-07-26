# Tidy Tuesday: 2021 Week 30
# 20 July 2021: Droughts
# This codes creates a map of the USA showing drought level on a certain date
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(RColorBrewer)
library(extrafont)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

modified_drought =
  drought %>% filter(valid_start == as.Date('2021-07-13') & area_pct > 0) %>%
  mutate(area_pct = round(area_pct, 0),
         drought_lvl = factor(drought_lvl, ordered=TRUE, levels=c('None', 'D1', 'D2', 'D3', 'D4'))) %>%
  uncount(area_pct)
  
rne_map <- ne_states(returnclass = "sf", country='United States of America')

rne_map %>% left_join(modified_drought, by=c('postal' = 'state_abb')) %>%
  ggplot(aes(x=longitude, y=latitude, group=drought_lvl)) +
  geom_sf(color = NA) +
  stat_density2d(aes(fill=drought_lvl, alpha=..density..), n=250, geom = "tile", contour = FALSE) +
  xlim(-170, -60) + ylim(15, 75) +
  scale_fill_manual(labels=c('None', 'Moderate Drought','Severe Drought', 'Extreme Drought', 'Exceptional Drought'),
                    values = c(rev(brewer.pal(5, "Spectral")))) +
  labs(title = 'Drought Levels across the USA',
       subtitle = 'Date: 13 July 2021',
       fill = '',
       caption='Data Source: The U.S. Drought Monitor \n(National Drought Mitigation Center,\n US Department of Agriculture and NOAA)\n Created by: dosullivan019') +
  scale_alpha_continuous(range=c(0,1), guide='none') + 
  theme_void() +
  theme(legend.position = 'top',
        plot.title = element_text(hjust=0.5, family='Georgia'),
        plot.subtitle = element_text(hjust=0.5, family='Georgia'),
        plot.caption = element_text(face='italic', size=8)) + 
  guides(fill = guide_legend(title.position = "top",
                             label.theme = element_text(family='Georgia', size=8)))


ggsave('plots/2021W30_drought.png')

                    