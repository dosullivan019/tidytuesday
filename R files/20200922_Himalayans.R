# Tidy Tuesday 22/09/2020
# Himalayan Climbers
# Libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(extrafont)

# TODO: Add legend to plot, play around with title colours and font, maybe change colour of bar plot labels

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

# extract mt everest data
everest <- members[which(members$peak_name=='Everest'),]

# Define colours for scatter plot
inj_col = ifelse(everest$solo, 'forestgreen', 'midnightblue') # green if solo
death_col = ifelse(everest$solo, 'forestgreen', 'darkorange2') # green if solo
size_solo = ifelse(everest$solo, 6,3) # solo to have a bigger size

# Plot scatter of height of deaths and injuries
everest_scatter <- everest %>% 
  ggplot(aes(x=year,y=highpoint_metres)) +
  geom_point(aes(x=year, y=injury_height_metres),col=inj_col,pch=16, size=size_solo) +
  geom_point(aes(x=year, y=death_height_metres),col=death_col, pch=17, size=size_solo) +
  annotate('label',x=unlist(everest[which(everest$solo & everest$died),'year']), 
           y=unlist(everest[which(everest$solo & everest$died),"death_height_metres"])-250,
           label='Solo', fill='dimgrey', colour='white') +
  annotate('label',x=min(everest[which(everest$died),'year'],na.rm=TRUE)+2, 
                   y=min(everest[which(everest$died),"death_height_metres"],na.rm=TRUE),
                   label='died', fill='lightgrey', colour='darkorange2',size=5) +
  annotate('label',x=2003, 
           y=min(everest[which(everest$injured),"injury_height_metres"],na.rm=TRUE)-50,
           label='injured', fill='lightgrey', colour='midnightblue', size=5) +
  geom_hline(yintercept = 8850, lty=2, col='dimgrey') +
  annotate('label',x=1950, y=8850, size=5,
           label='Summit', fill='dimgrey', colour='white') +
  ylim(c(0,9000)) +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        axis.title.x = element_text(colour='black',family="Lucida Calligraphy"),
        axis.text.x = element_text(colour='black'),
        axis.title.y = element_text(colour='black',family="Lucida Calligraphy"),
        axis.text.y = element_text(colour='black'),
        plot.title=element_text(size=30,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.subtitle=element_text(size=15,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy")) +
  labs(title = 'Mount Everest', subtitle = 'Height of injuries and deaths') + xlab('Year') + ylab ('Height (m)')


# Plotting top 5 cause of injury
top5_injury_cause <- 
  everest %>% filter(injured==TRUE) %>%
  select(injured, injury_type) %>% 
  group_by(injury_type) %>% count() %>%
  arrange(.,-n) %>% ungroup() %>% 
  dplyr::slice_head(., n=5, order_by=-n) %>%
  ggplot(aes(x=reorder(injury_type, n),y=n)) + 
  geom_bar(stat='identity',fill='grey55')  + coord_flip() +
  labs(title = 'Top 5', subtitle = 'Causes of Injury') +
  geom_text(aes(y=ifelse(n>20,n,n+5), label=n), size=4.5, hjust=1.1, col='black') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12, colour='black',family="Lucida Calligraphy"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title=element_text(hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.subtitle=element_text(size=12,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"))

# Plotting top 5 roles of injured
top5_injury_roles <- 
  everest %>% filter(injured==TRUE) %>%
  select(injured, expedition_role) %>% 
  group_by(expedition_role) %>% count() %>%
  arrange(.,-n) %>% ungroup() %>% 
  dplyr::slice_head(., n=5, order_by=-n) %>%
  ggplot(aes(x=reorder(expedition_role, n),y=n)) + 
  geom_bar(stat='identity',fill='grey55') + coord_flip() +
  labs(title = 'Top 5', subtitle = 'Expedition Roles of Injured') +
  geom_text(aes(y=ifelse(n>20,n,n+30), label=n), size=4.5, hjust=1.1, col='black') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=11, colour='black',family="Lucida Calligraphy"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title=element_text(hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.subtitle=element_text(size=12,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"))

# Plotting top 5 cause of deaths
top5_death_cause <- everest %>% filter(died==TRUE) %>%
  select(died, death_cause) %>% 
  group_by(death_cause) %>% count() %>%
  arrange(.,-n) %>% ungroup() %>% 
  dplyr::slice_head(., n=5, order_by=-n) %>%
  ggplot(aes(x=reorder(death_cause, n),y=n)) + 
  geom_bar(stat='identity',fill='grey55') + coord_flip() +
  labs(title = 'Top 5', subtitle = 'Causes of Death') +
  geom_text(aes(y=ifelse(n>20,n,n+5), label=n), size=4.5, hjust=1.1, col='black') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=11, colour='black',family="Lucida Calligraphy"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title=element_text(hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.subtitle=element_text(size=12,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"))

# Plotting top 5 roles of people who died
top5_death_roles <- everest %>% filter(died==TRUE) %>%
  select(died, expedition_role) %>% 
  group_by(expedition_role) %>% count() %>%
  arrange(.,-n) %>% ungroup() %>% 
  dplyr::slice_head(., n=5, order_by=-n) %>%
  ggplot(aes(x=reorder(expedition_role, n),y=n)) + 
  geom_bar(stat='identity',fill='grey55') + coord_flip() +
  labs(title = 'Top 5', subtitle = 'Expedition Roles of Deaths', caption='Data Source: The Himalayan Database') +
  geom_text(aes(y=ifelse(n>20,n,n+10), label=n), size=4.5, hjust=1.1,col='black') +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=11, colour='black',family="Lucida Calligraphy"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill='lightgrey',color=NA),
        panel.background = element_rect(fill='lightgrey'),
        plot.title=element_text(hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.subtitle=element_text(size=12,hjust=0.5, face="bold", colour="black",family="Lucida Calligraphy"),
        plot.caption=element_text(size=8,hjust=1, face="bold", colour="black",family="Lucida Calligraphy"))


# Arranging top5 graphs
top5 <- ggarrange(top5_injury_cause, top5_injury_roles, top5_death_cause, top5_death_roles,
          ncol=4, nrow = 1) 
buffer = ggplot() + 
  theme(panel.background = element_rect(fill='lightgrey'),
        panel.border=element_blank(),
        plot.background = element_rect(fill='lightgrey'),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = unit(c(0,0,0,0), 'cm'))
# Arranging with the scatter plot
ggarrange(everest_scatter, buffer, top5, ncol=1, nrow=3, heights = c(2.5,0.05,1)) + 
  bgcolor('lightgrey') + border(color='lightgrey')
ggsave('plots/HimalayanClimbers.png', width=16,height=10)
