# Tidy Tuesday: 2021 Week 37
# 6 September 2021: F1 Racing
# This codes creates a line chart comparing Lewis Hamilton, Michael Schumacher and Max Verstappens wins throughout their careers
library(dplyr)
library(ggplot2)
library(extrafont)

driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')

driver_standings %>% left_join(drivers, by = c('driverId' = 'driverId')) %>%
  left_join(results, by = c('raceId' = 'raceId', 'driverId' = 'driverId')) %>%
  left_join(races, by = c('raceId' = 'raceId')) %>%
  filter(surname %in% c('Hamilton', 'Schumacher', 'Verstappen') & forename %in% c('Lewis', 'Michael', 'Max')) %>%
  arrange(year, round) %>%  # order by year and round so the races are in chronological order.
  select(year, round, wins, points.x, points.y, position.x, position.y, surname) %>%
  group_by(surname) %>%
  mutate(race_number=row_number(),                                    # calculate the race number of each drivers career
         race_position = as.numeric(position.y),                      # change race position to numeric
         count_wins = if_else(race_position == 1, 1, 0, missing = 0), # Keep track of each win
         cumulative_wins = cumsum(count_wins)) %>%                    # Sum up the total number of wins
  ggplot() +
  geom_line(aes(x=race_number, y=cumulative_wins, group=surname, colour=surname), size=1) +
  scale_colour_manual(values=c('darkturquoise', 'red2', 'black')) +
  labs(title = 'Hamilton, Schumacher and Verstappen: Cumulative F1 wins over time
       ',
       subtitle = 'Max Verstappen was slower to start winning races but has recently had a lot of wins and is \nstarting to catch up with Lewis Hamilton',
       x = 'Race Number',
       y = 'Cumulative Wins') +
  theme_minimal() +
  theme(plot.title = element_text(colour = 'white', family='Georgia', size = 16),
        plot.subtitle = element_text(colour = 'white', family='Georgia', size = 14),
        axis.title = element_text(colour = 'white', family='Georgia'),
        axis.text = element_text(colour = 'white', family='Georgia'),
        legend.text = element_text(colour = 'white', family='Georgia', size=12),
        legend.position = 'top',
        legend.title = element_blank(),
        plot.background = element_rect(fill='grey25'),
        panel.background = element_rect(fill='grey25', colour='grey25'),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

ggsave('plots/2021W37_F1Racing.png')  
