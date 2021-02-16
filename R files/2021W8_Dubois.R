# Tidy Tuesday: 2021 Week 8
# 16 Feb 2021: DuBois Challenge
library(tidyr)
library(ggplot2)
library(gt)
library(ggpubr)

income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')

stacked_bar_chart <- income %>% gather(., key='expense', value='percent', Rent:Other) %>% 
  mutate(expense=factor(expense, levels=c('Other', 'Tax', 'Clothes', 'Food', 'Rent'), ordered=TRUE),
         Class=factor(Class, levels=c('Over $1000', '$750-1000','$500-750', '$400-500', 
                                      '$300-400', '$200-300','$100-200'), ordered=TRUE),
         ) %>%
  ggplot(aes(x=Class, y=percent, fill=expense)) + geom_bar(stat='identity') + 
  geom_text(aes(label=percent), position='stack', size=4.5, hjust=1.2, col='white') +
  coord_flip() + 
  scale_fill_manual(values=c('gray80', 'cadetblue1', 'orange', 'mediumorchid1', 'black')) +
  labs(title='Income and Expenditure of 150 Negro families in Atlanta, GA, USA') +
  theme_void() + theme(legend.position='none')

gt_table <- income[, 1:2] %>% gt() %>% fmt_currency(columns = vars(`Actual Average`), currency='EUR') %>%
  tab_style(cell_borders(sides='all'), 
            locations = cells_body(
              columns = everything(),
              rows = everything()
            )) %>% ggplot_image(stacked_bar_chart)

ggarrange(gt_table, stacked_bar_chart, ncol=2, nrow=1, heights = c(2.5,2.25)) + 
  bgcolor('lightgrey') + border(color='lightgrey')
