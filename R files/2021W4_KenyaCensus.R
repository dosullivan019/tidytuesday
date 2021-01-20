# Tidy Tuesday: 2021 Week 4
# 19 Jan 2021: Kenya Census


library(ggplot2)
library(dplyr)
library(rKenyaCensus)
library(sf)
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

## Extract shapefile
counties_sf = KenyaCounties_SHP %>% sf::st_as_sf()

# Make county names upper for join
households = households[-which(households$County=='Kenya'),]
households$County = toupper(households$County)

# Join shapefile to households data and plot
left_join(counties_sf, households, by='County') %>% 
  ggplot(aes(fill = AverageHouseholdSize)) +
  geom_sf(color = NA) +
  scale_fill_gradient(high='navy',low='darkturquoise',na.value = 'lightgrey') + 
  theme_void() + labs(title = "Kenya Household Size",
                      fill = "Average Household Size",
                      caption = "Data source: rKenyaCensus \n Created by: dosullivan019") +
  theme(plot.title = element_text(size=18,face='bold', colour='dimgrey', hjust=0.5),
        legend.position = 'top', legend.title=element_text(size=12, vjust=0.75, colour='dimgrey'),
        legend.text = element_text(size=10, colour='dimgrey'), legend.key.height = unit(0.7, "cm"),
        plot.caption=element_text(size=8,hjust=1, vjust=1, face="italic", colour="dimgrey")
        )
ggsave(filename='2021W4_KenyaCensus.png', path='./plots')
