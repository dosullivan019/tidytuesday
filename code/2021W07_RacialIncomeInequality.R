# Tidy Tuesday: 2021 Week 7
# 09 Feb 2021: Income Inequality in the USA
library(dplyr)
library(ggplot2)
library(wesanderson)
library(ggpubr)

home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')

# Plot yearly income for each race
race_wealth_plot = race_wealth %>% filter(type=='Median') %>%
  ggplot(aes( x=year, y=wealth_family, col=race)) +
  geom_line(size=1) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3, prefix = "$")) +
  scale_colour_manual(name='Race',
                      values=c(wes_palettes$Zissou1[1], wes_palettes$Moonrise1[4],wes_palettes$Moonrise2[1],wes_palettes$Zissou1[4])) +
  labs(title='Racial Income Inequality in the USA',
       subtitle = 'Racial difference in median income by race 1963-2016',
       x='Year', y='Median Income', legend='Race') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line = element_line(colour = "gray30"),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title = element_text(size=22,face='bold', colour='gray15'),
        plot.subtitle = element_text(size=16, face='bold',colour='gray30'),
        legend.background = element_rect(fill='lightgrey',color=NA),
        legend.key = element_rect(fill = "lightgrey", colour = NA),
        legend.text = element_text(colour='gray30', size=10),
        legend.position = 'bottom',
        axis.title.x = element_text(colour='gray30', size=10, face='bold'),
        axis.text.x = element_text(colour='gray30', size=10),
        axis.title.y = element_text(colour='gray30', size=10, face='bold'),
        axis.text.y = element_text(colour='gray30', size=10))

# plot yearly home ownership % per race
home_owner_plot = home_owner %>% 
  ggplot(aes( x=year, y=home_owner_pct, col=race)) +
  geom_line(size=1) +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e+2), limits = c(0,1)) +
  scale_colour_manual(values=c(wes_palettes$Zissou1[1], wes_palettes$Moonrise1[4], wes_palettes$Zissou1[4])) +
  labs(title='Racial difference in home ownership',
       caption='Data Source: Urban Institute and US Census \n Created by: dosullivan019',
       x='Year', y='% home owners') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line = element_line(colour = "gray30"),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title = element_text(size=16,face='bold', colour="gray30"),
        plot.caption=element_text(size=12,hjust=1, vjust=1, face="italic", colour="gray10"),
        legend.position = 'none',
        axis.title.x = element_text(colour='gray30', size=10, face='bold'),
        axis.text.x = element_text(colour='gray30', size=10),
        axis.title.y = element_text(colour='gray30', size=10, face='bold'),
        axis.text.y = element_text(colour='gray30', size=10))

# Arranging both graphs
ggarrange(race_wealth_plot, home_owner_plot, ncol=1, nrow=2, heights = c(2.5,2.25)) + 
  bgcolor('lightgrey') + border(color='lightgrey')
ggsave('plots/2021W7_RacialIncomeInequality.png', width=12,height=10)
