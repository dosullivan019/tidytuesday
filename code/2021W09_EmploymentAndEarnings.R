# Tidy Tuesday: 2021 Week 9
# 23 Feb 2021: Employment and Earnings
library(ggplot2)
library(dplyr)
library(extrafont)

# loadfonts(device='win')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')
# Combine year and quarter into one variable for plotting
earn['year_qtr'] = factor(paste(earn$year,earn$quarter,sep=' Q'),ordered=TRUE)

# plot to show difference in earnings between women of different ethnic groups
earn %>% filter(sex=='Women' & race!='All Races' & 
                  age %in% c('16 to 24 years', '25 to 54 years', '55 years and over')) %>%
  ggplot(aes(x=year_qtr, y=median_weekly_earn, col=race, group=race)) + 
  geom_line(size=1) + 
  facet_wrap(facets=vars(age)) +
  labs(title='Median Weekly Earnings of Women',
       subtitle = 'Asian women tend to earn more than other ethnic groups in the 16-54 age group.\nBlack or African American women have the lowest earnings.',
       x='Year',
       y='Median Weekly Earnings ($)',
       colour='Race',
       caption = 'Data Source: US Bureau of Labor Statistics\nCreated by: dosullivan019') +
  scale_x_discrete(breaks=levels(earn$year_qtr)[floor(seq(1,nlevels(earn$year_qtr)-3, length.out = 11))],
                   labels=gsub(" Q1", "", levels(earn$year_qtr)[floor(seq(1,nlevels(earn$year_qtr)-3, length.out = 11))])) +
  scale_colour_manual(values=c('grey61','cornflowerblue','gold')) +
  theme(legend.position = 'top',
        legend.key.width = unit(1.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        plot.title=element_text(size=30, face='bold', colour="black",family="Bodoni MT"),
        plot.subtitle=element_text(size=18, colour="black",family="Bodoni MT"),
        plot.caption=element_text(size=7, face="italic"),
        panel.background = element_rect(fill='grey97'),
        axis.title.x = element_text(colour='black',family="Bodoni MT",size=16),
        axis.text.x = element_text(colour='black',size=10),
        axis.title.y = element_text(colour='black',family="Bodoni MT",size=16),
        axis.text.y = element_text(colour='black',size=10),
        legend.title = element_text(size=16, family='Bodoni MT'),
        legend.text = element_text(size=16, family='Bodoni MT'),
        strip.text.x = element_text(size = 14, family='Bodoni MT') # modify facet text
        )

ggsave('plots/2021W9_EmploymentAndEarnings.png',width=14.98, height=7)
