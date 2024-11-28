# libraries used
library(dplyr)
library(ggplot2)

# setting working directory
setwd("~/Dropbox/Nationalities Project")

# reading in data
nationalities <- read.csv('nationalities.csv', row.names = 1)

# calculating percentages
nationalities2 <- nationalities %>% group_by(season, league) %>%
  mutate(PercPlayers = 100*NumPlayers/sum(NumPlayers), PercMin = 100*Min/sum(Min))

home_players <- nationalities2 %>%
  filter(Nation %in% c("eng England", "ie Republic of Ireland", "sct Scotland", 
                       "wls Wales", "nir Northern Ireland")) %>%
  select(c(2,3,4,6,7,8,9)) %>%
  group_by(season, league)%>%
  summarise(TotPercPlayers = sum(PercPlayers), TotPercMin = sum(PercMin))%>%
  mutate(season = paste(substring(season, 3, 5), substring(season, 8, 9), sep = '') ) 


plt <- ggplot(home_players, aes(x = season, y = TotPercPlayers, colour = league, group = league)) +
  geom_line(linewidth = 1.6, alpha = 0.8) +
  labs(title = "% of 🇬🇧﹠🇮🇪 Players in Prem & EFL", x = "Season", y = "% Players", 
       subtitle = "Source: FBREF", caption = "SportsVisualised") +
  theme_minimal()  +
  scale_color_manual(labels = c("Premier League", "Championship", "League 1", "League 2"), 
                     values = c("#fa8d05","#34f708", "#07e2f8", "#5203fc"))  +
  theme(
    aspect.ratio=0.75,
    legend.position = "top", legend.title = element_blank(),
    text = element_text(family = "Avenir", color = tcolor),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = tcolor),
    axis.text.y = element_text(size = 12, color = tcolor),
    axis.title.x = element_text( size = 17, color = tcolor),
    axis.title.y = element_text( size = 17, color = tcolor),
    plot.title = element_text( size = 22, face = "bold", color = tcolor, hjust = 0),
    plot.caption = element_text(size = 17, family = "Tektur"),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 1),
    panel.background = element_rect(fill = bcolor, color = NA), 
    plot.background = element_rect(fill = bcolor, color = NA), 
    panel.grid.major = element_line(color = tcolor, size = 0.1),    
    panel.grid.minor = element_line(color = tcolor, size = 0.1),  
  ) + expand_limits(y = c(0,100) )
ggsave('homeplayersplot.png', plt, width = 6, height = 6, units = "in", scale = 1)


# Minutes Plot
plt <- ggplot(home_players, aes(x = season, y = TotPercMin, colour = league, group = league)) +
  geom_line(linewidth = 1.6, , alpha = 0.8) +
  labs(title = "% of 🇬🇧﹠🇮🇪 Minutes in Prem & EFL", x = "Season", y = "% Minutes", 
       subtitle = "Source: FBREF", caption = "SportsVisualised") +
  theme_minimal()  +
  scale_color_manual(labels = c("Premier League", "Championship", "League 1", "League 2"), 
                     values = c("#fa8d05","#34f708", "#07e2f8", "#5203fc")) +
  theme(
    aspect.ratio=0.75,
    legend.position = "top", legend.title = element_blank(),
    text = element_text(family = "Avenir", color = tcolor),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = tcolor),
    axis.text.y = element_text(size = 12, color = tcolor),
    axis.title.x = element_text( size = 17, color = tcolor),
    axis.title.y = element_text( size = 17, color = tcolor),
    plot.title = element_text( size = 22, face = "bold", color = tcolor, hjust = 0),
    plot.caption = element_text(size = 17, family = "Tektur"),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 1),
    panel.background = element_rect(fill = bcolor, color = NA), 
    plot.background = element_rect(fill = bcolor, color = NA),  
    panel.grid.major = element_line(color = tcolor, size = 0.1),    
    panel.grid.minor = element_line(color = tcolor, size = 0.1),  
  ) + expand_limits(y = c(0,100) )
ggsave('homeminsplot.png', plt, width = 6, height = 6, units = "in", scale = 1)
