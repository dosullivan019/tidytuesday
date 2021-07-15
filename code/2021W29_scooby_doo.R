# Tidy Tuesday: 2021 Week 29
# 13 July 2021: Scooby Doo
# This codes create a sankey showing which Scooby Doo character captures which type of monster

library(ggplot2)
library(ggsankey)
library(dplyr)
library(tidyr)
library(extrafont)
library(wesanderson)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

colour_palette = c(wes_palettes$Darjeeling2, wes_palettes$GrandBudapest2, wes_palettes$GrandBudapest1[2:3])
pie(rep(1, 11), col=colour_palette)
scoobydoo %>% mutate(across(starts_with("caught_"), factor)) %>%
  pivot_longer(cols=starts_with('caught_'),
                           names_to = "caught_by",
                           names_prefix = "caught_",
                           values_to = "caught_by_logi") %>%
  filter(caught_by_logi == TRUE & !(caught_by %in% c('other', 'not')) &
           monster_type %in% c("Ghost", "Super-Villain", "Magician", "Mummy",
                               "Animal", "Mechanical", "Extraterrestrial")) %>%
  make_long(caught_by, monster_type) %>% 
  ggplot(aes(x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = factor(node),
      label = node)) +
  geom_sankey(flow.alpha = .6) +
  geom_sankey_label(size = 4, color = "white", fill = "gray30") +
  scale_fill_manual(values=colour_palette)+
  labs(title = 'Scooby Doo: Who caught who?',
       subtitle = 'Which characters caught which monster type',
       caption='Data Source: Kaggle\n Created by: dosullivan019') +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family='Goudy Stout', colour = wes_palettes$Darjeeling2[2], hjust=0.5),
        plot.subtitle = element_text(family='Goudy Stout', colour = wes_palettes$GrandBudapest2[4], hjust=0.5),
        plot.caption = element_text(face='italic'))

ggsave('plots/2021W29_scooby_doo.png')
