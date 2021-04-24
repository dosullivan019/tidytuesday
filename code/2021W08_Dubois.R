# Tidy Tuesday: 2021 Week 8
# 16 Feb 2021: Dubois Challenge
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')

# Cleaning NA to match the original plot
income[is.na(income$Other),'Tax'] = NA
income[is.na(income$Other),'Other'] = 9.9

stacked_bar_chart = income %>% gather(., key='expense', value='percent', Rent:Other) %>% 
  mutate(expense=factor(expense, levels=c('Other', 'Tax', 'Clothes', 'Food', 'Rent'), ordered=TRUE),
         Class=factor(Class, levels=c('Over $1000', '$750-1000','$500-750', '$400-500', 
                                      '$300-400', '$200-300','$100-200'), ordered=TRUE)) %>%
  ggplot(aes(x=Class, y=percent)) + 
  geom_bar(aes(fill=expense), stat='identity') + 
  scale_fill_manual(values=c('gray80', 'cadetblue1', 'lightpink', 'mediumorchid1', 'black')) +
  geom_text(aes(label=scales::percent(ifelse(percent>0,percent,NA), accuracy=1, scale=1),
            colour=expense),
            stat='identity', position=position_stack(0.5), size=4.5) +
  scale_colour_manual(values=c('black', 'black', 'black', 'black','white'))+
  labs(title='INCOME AND EXPENDITURE OF 150 NEGRO FAMILIES IN ATLANTA, GA, USA') +
  coord_flip() +
  theme_void() + 
  theme(plot.background = element_rect(fill='antiquewhite',color=NA),
        plot.title = element_text(size=20,hjust=0.02, colour='black'),
        legend.position = 'top',
        legend.key.width = unit(6.8, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.title=element_blank()) +
  guides(fill = guide_legend(label.position = "top",label.theme=element_text(size=12), reverse=TRUE, direction='horizontal'),
         colour = FALSE)

table_chart = income %>% ggplot(aes(x=rep(1, 7), 
                                    y=factor(Class, 
                                              levels=c('Over $1000', '$750-1000','$500-750', '$400-500', 
                                              '$300-400', '$200-300','$100-200'), ordered=TRUE))) +
   geom_tile(fill='antiquewhite') + xlim(0.8, 1.2) + 
  geom_text(aes(label = `Actual Average`), size=5) + 
  ggtitle('CLASS') +
  annotate('text',label='ACTUAL AVERAGE', x=1, y=7.5, fontface='bold') +
  theme(plot.title = element_text(hjust = -0.25, vjust=-7),
        plot.background = element_rect(fill='antiquewhite',color=NA),
        panel.background =element_rect(fill='antiquewhite',color=NA),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12, face='bold', colour='black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(2,1,1,1), 'lines'))

ggarrange(table_chart, stacked_bar_chart, ncol=2, nrow=1, widths = c(1,3))
ggsave('plots/2021W8_DuboisChallenge.png', width=20,height=10)
