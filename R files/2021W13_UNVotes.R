# Tidy Tuesday: 2021 Week 13
# 23 Mar 2021: UN Votes
library(tidyr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(extrafont)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# Join data
vote_issues <- left_join(unvotes, issues)
roll_votes <- left_join(unvotes, roll_calls)

# Creating map using rnaturalearth
rne_map <- ne_countries(scale = "medium", returnclass = "sf")  

# Create Map of Votes
left_join(rne_map, roll_votes, by=c('postal'='country_code')) %>% 
  filter(session==74 & (grepl('racism',descr) | grepl('Oceans and the law of the sea',descr,fixed=TRUE) |
                          grepl('Towards a nuclear-weapon-free world',descr, fixed=TRUE) | 
                          grepl('Agricultural technology for sustainable development',descr, fixed=TRUE)| 
                          grepl('Transparency and confidence-building measures in outer space activities',descr, fixed=TRUE))) %>%
  mutate(vote=as.factor(vote),
         short=gsub('A global call for concrete action for the total elimination of racism, racial discrimination, xenophobia and related intolerance and the comprehensive implementation of and follow-up to the Durban Declaration and Programme of Action',
                    'A global call for concrete action for the total elimination of racism..',
                    gsub('Combating glorification of Nazism, neo-Nazism and other practices that contribute to fuelling contemporary forms of racism, racial discrimination, xenophobia and related intolerance', 
                    'Combating glorification of Nazism, neo-Nazism..', short)
                    )
        ) %>%
  ggplot(aes(fill=vote)) + 
  geom_sf(color = NA) + facet_wrap(vars(short), ncol=2,
                                   labeller = label_wrap_gen(width=40)) + 
  scale_fill_manual(values=c('grey59', 'royalblue', 'skyblue'), na.value = 'lightgrey') +
  labs(title='UN Votes 2019',
       subtitle = 'USA were one of few countries who voted against anti-racism,\na nuclear weapon free world and transparency in outer space advancements.',
       fill='Vote',
       caption = 'Data Source: Harvard Dataverse\nCreated by: dosullivan019') +
  theme(legend.position = 'top',
        panel.background = element_rect(fill='grey97'),
        plot.title=element_text(size=22, face='bold', colour="black",family="Bodoni MT"),
        plot.subtitle=element_text(size=16, colour="black",family="Bodoni MT"),
        plot.caption=element_text(size=7, face="italic"),
        legend.title = element_text(size=12, family='Bodoni MT'),
        legend.text = element_text(size=12, family='Bodoni MT'),
        strip.text.x = element_text(size = 12, family='Bodoni MT'))

ggsave('plots/2021W13_UN_Votes.png', width=14, height=7)
