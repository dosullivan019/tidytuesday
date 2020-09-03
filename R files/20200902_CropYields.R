# Tidy Tuesday: Tuesday Sept 1st 2020
# Key Crop Yields
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# Looking at cocoa beans production
cocoa_crop <- key_crop_yields %>% select(Entity, Code, Year, `Cocoa beans (tonnes per hectare)`) %>% 
                                  filter(!(is.na(`Cocoa beans (tonnes per hectare)`) | is.na(Code) ))

# Find top 15 countries for each year
cocoa_top10 <- cocoa_crop %>% group_by(Year) %>% 
                  mutate(rank = rank(`Cocoa beans (tonnes per hectare)`)) %>%
                  group_by(Entity) %>% 
                  filter(rank <=15)

# Static Bar plot of top 15
cocoa_bar_plot <- ggplot(cocoa_top10, aes(x=rank, group_by(Entity), fill=as.factor(Entity), color = as.factor(Entity))) +
  geom_tile(aes(y = `Cocoa beans (tonnes per hectare)`/2,
                height = `Cocoa beans (tonnes per hectare)` ,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Entity, " ")), vjust=0.3, hjust=1.25) + 
  coord_flip() +
  guides(color = FALSE, fill = FALSE) +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
       panel.border=element_blank(),
       panel.grid.major=element_blank(),
       panel.grid.minor=element_blank(),
     #  panel.grid.major.x = element_line( size=.1, color="grey" ),
      # panel.grid.minor.x = element_line( size=.1, color="grey" ),
       plot.background = element_blank(),
       plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
       plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="grey"),
       plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
       plot.margin = margin(2,2, 2, 4, "cm"))

# TO DO: Make country labels more clear, add value labels at other end of x-axis
# get the frames and pace of gif correct, moving too fast and ridgedy


# Animate the bar chart to change each year
anim = cocoa_bar_plot + transition_states(Year, transition_length = 2, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Cocoa Yield (tonnes per hectare) : {closest_state}',  
       subtitle  =  "Top 15 Countries",
       caption  = "Cocoa Yield (tonnes per hectare) | Data Source: Our World in Data"
      )

# With the animation being built (ready) and saved in the object anim , It’s time for us to render the animation using animate() function. The renderer used in the animate() differs based on the type of output file required.
# For GIF File Format:
animate(anim, 200, fps = 3,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
