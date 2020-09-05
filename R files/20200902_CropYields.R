# Tidy Tuesday: Tuesday Sept 1st 2020
# Key Crop Yields
# This script creates an animated bar plot of the top 5 countries for potatoe yield each year
library(dplyr)
library(ggplot2)
library(gganimate)  # for animation
library(gifski)     # for creating gif
library(pals)       # for colours

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# Belgium is also called Belgium-Luxembourg before 2000 so will rename these to Belgium to get a better view
key_crop_yields[which(key_crop_yields$Entity == 'Belgium-Luxembourg'), 'Entity'] = 'Belgium'

# Looking at potatoe yield
potatoes <- key_crop_yields %>% select(Entity, Code, Year, `Potatoes (tonnes per hectare)`) %>% 
                                  filter(!(is.na(`Potatoes (tonnes per hectare)`) | is.na(Code) ))

# Find top 5 countries for each year
potatoes_top5 <- potatoes %>% group_by(Year) %>% 
                  mutate(rank = rank(-`Potatoes (tonnes per hectare)`) # minus so they are ranked greatest yield = rank 1
                         ) %>%
                  group_by(Entity) %>% 
                  filter(rank <=5) # only look at the top 5

# Static Bar plot of top 5
potatoe_bar_plot <- ggplot(potatoes_top5, aes(x=rank, group_by(Entity), fill=as.factor(Entity), 
                                               color = as.factor(Entity)
                                               )) +
  geom_tile(aes(y = `Potatoes (tonnes per hectare)`/2,
                height = `Potatoes (tonnes per hectare)` ,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Entity, " ")), vjust=0.25, hjust=0.95, size=10) + # country labels
  geom_text(aes(y=`Potatoes (tonnes per hectare)`, label=paste(' ', round(`Potatoes (tonnes per hectare)`,0)))
            , hjust=0, size=8) + # value label at the end of the bar
  scale_y_continuous(limits = c(-10, max(potatoes_top10$`Potatoes (tonnes per hectare)`))) +
  scale_colour_manual(values=kovesi.linear_ternary_blue_0_44_c57(length(unique(potatoes_top10$Entity)))) + 
  scale_fill_manual(values=kovesi.linear_ternary_blue_0_44_c57(length(unique(potatoes_top10$Entity)))) + 
  coord_flip(expand=TRUE) + scale_x_reverse() + 
  theme_minimal() +
  theme(legend.position = 'none',
        axis.line=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_blank(),
        plot.title=element_text(size=30, vjust=0.5, hjust=0.25, face="bold", colour="gray32"),
        plot.subtitle=element_text(size=25, vjust=0.5, hjust=0.25, face="italic", color="gray32"),
        plot.caption =element_text(size=12, hjust=0.75, face="italic", color="gray32"),
        plot.margin = margin(1, -7, 1, 1, "cm"))

# Animate the bar chart to change each year
potatoe_anim = potatoe_bar_plot + transition_states(Year, transition_length = 3, state_length = 1.5) +
  labs(title = 'Potatoe Yield (tonnes per hectare) : {closest_state}',  
       subtitle  =  "Top 5 Countries",
       caption  = "Data Source: Our World in Data"
      )

# With the animation being built (ready) and saved in the object anim , It’s time for us to render the animation using animate() function. The renderer used in the animate() differs based on the type of output file required.
# For GIF File Format:
animate(potatoe_anim,350, fps = 6,  width = 1200, height = 1000, 
        renderer = gifski_renderer("plots/potatoe_animation.gif"), rewind=FALSE, end_pause=5)

