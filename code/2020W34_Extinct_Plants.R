# Tidy Tuesday
# 18/08/2020: Plants in Danger
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(wesanderson)
# plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
# actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

sapply(threats, function(x) sum(is.na(x))) # year_last_seen is only variable with na's

# Calculating the percentage of plants classified as threatened in each country
threat_pct <- threats %>% group_by(country) %>%
  summarise_at(., 'threatened', function(x) (sum(x)/length(x) * 100)) %>% arrange(-threatened)
colnames(threat_pct) <- c('name', 'pct_threatened')

# Creating map using rnaturalearth
rne_map <- ne_countries(scale = "medium", returnclass = "sf")

# Plotting % plants threatened
pct_threatened_map = 
  left_join(rne_map, threat_pct, by='name') %>% 
  ggplot(aes(fill =pct_threatened)) + 
  geom_sf(color = NA) +
  scale_fill_gradient(high='navy',low='darkturquoise',na.value = 'lightgrey') + 
  theme_void() + labs(title = "Plants in Danger",
                       subtitle = "% of observed plants threatened per country",
                        fill = "% plants threatened",
                       caption = "Data source: IUCN Red List") +
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'), 
        plot.subtitle = element_text(hjust = 0.5,size=12), 
        legend.position = 'top', legend.title=element_text(vjust = 0.75, size=8,face='bold'),
        legend.text = element_text(size=5), legend.key.height = unit(0.25, "cm"))
ggsave(filename='20200818_PlantsInDanger_PercentageThreatenedMap.png', plot=pct_threatened_map, path='./plots')

# Calculating the greatest threat_type of threatened plants in each country
# Need the number of plants threatened by country and threat type
# Then find the threat which has the max number of plants threatened for each country
threats_for_vz <- threats %>% 
  group_by(country) %>% mutate(total_cntry_sp = length(country)) %>%
  group_by(country) %>% mutate(pct_cntry_sp_threatened = sum(threatened)/length(country) * 100) %>% 
  group_by(country, threat_type) %>% mutate(total_sp_threat_type_cntry=sum(threatened)) %>% 
  group_by(country) %>% mutate(greatest_threat_cntry=max(total_sp_threat_type_cntry))

threat_type_vz <- 
  unique(threats_for_vz[which(threats_for_vz$total_sp_threat_type_cntry==threats_for_vz$greatest_threat_cntry),
               c('country', 'threat_type', "total_sp_threat_type_cntry", "greatest_threat_cntry")] ) %>% 
  group_by(country) %>% mutate(cnt_threat = length(country)) %>% 
  # if a country has more than one threat type being the biggest threat then rename as 'Multiple Factors'
  mutate(greatest_threat=ifelse(cnt_threat>1, 'Multiple Factors', threat_type)) %>% 
  group_by(country) %>% select(country,greatest_threat)
colnames(threat_type_vz)=c('name','greatest_threat')

# Joining to map and plot
greatest_threat_map =
  left_join(rne_map, threat_type_vz, by='name') %>% 
  ggplot(aes(fill =greatest_threat)) + 
  geom_sf(color = NA) +
  scale_fill_manual(values=c(wes_palettes$Cavalcanti1, wes_palettes$Rushmore1),na.value='lightgrey') +
  theme_void() + labs(title = "Plants in Danger",
                      subtitle = "Most common threat which has caused plants to become threatened",
                      fill = "Threat",
                      caption = "Data source: IUCN Red List") +
  theme(plot.title = element_text(hjust = 0.5,size=15, face='bold'),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = 'top', legend.title=element_text(size=8,vjust = 0.75, face='bold'),
        legend.text = element_text(size=6), legend.key.height = unit(0.25, "cm"))
ggsave(filename='20200818_PlantsInDanger_CauseOfThreatenedMap.png', plot=greatest_threat_map, path='./plots')