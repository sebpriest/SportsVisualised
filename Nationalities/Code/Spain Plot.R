# libraries used
library(dplyr)
library(ggplot2)

# setting working directory
setwd("~/Dropbox/Nationalities Project")

# reading in data
nationalities <- read.csv('nationalities.csv', row.names = 1)

# filtering
spain_players <- nationalities %>%
  filter(`Nation` %in% c("es Spain")) %>%
  select( c(2,3,4,6,7)) %>%
  group_by(season) %>%
  summarise(NumPlayers = sum(NumPlayers),Min = sum(Min)) %>%
  mutate(season = paste(substring(season, 3, 5), substring(season, 8, 9), sep = '') )


# setting colours
bcolor <- '#00125B'
tcolor <-  '#c0cdff'


plt <- ggplot(spain_players, aes(x = season, y = NumPlayers, group = 1))  +
  geom_line(color = "#ff0015", linewidth = 1.2) + geom_line(color = "#ffe100", linewidth = 0.5) +   
  geom_vline(xintercept = '10-11', color = tcolor, linetype = "dashed") + 
  annotate("text",label = "World", x = "10-11", y = 62, color = "#ffe100", 
           family = "Avenir", size = 3.4) +
  geom_vline(xintercept = '08-09', color = tcolor, linetype = "dashed") + 
  annotate("text",label = "Euro", x = "08-09", y = 62, color = "#ffe100", 
           family = "Avenir", size = 3.4) +
  geom_vline(xintercept = '12-13', color = tcolor, linetype = "dashed") + 
  annotate("text",label = "Euro", x = "12-13", y = 62, color = "#ffe100", 
           family = "Avenir", size = 3.4) +  
  geom_vline(xintercept = '24-25', color = tcolor, linetype = "dashed") + 
  annotate("text",label = "Euro", x = "24-25", y = 62, color = "#ffe100", 
           family = "Avenir", size = 3.4) +  
  
  labs(title = "# of 🇪🇸 Players in Prem & EFL", x = "Season", y = "# Players", 
       subtitle = "Source: FBREF", caption = "SportsVisualised") +
  theme_minimal()  +
  theme(
    aspect.ratio=0.85,
    text = element_text(family = "Avenir", color = tcolor),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = tcolor),
    axis.text.y = element_text(size = 12, color = tcolor),
    axis.title.x = element_text( size = 17, color = tcolor),
    axis.title.y = element_text( size = 17, color = tcolor),
    plot.title = element_text( size = 24, face = "bold", color = tcolor, hjust = 0),
    plot.caption = element_text(size = 17, family = "Tektur"),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 1),
    panel.background = element_rect(fill = bcolor, color = NA), # Plot background
    plot.background = element_rect(fill = bcolor, color = NA),  # Overall plot background
    # Grid lines
    panel.grid.major = element_line(color = tcolor, linewidth = 0.1),    # Major grid lines
    panel.grid.minor = element_line(color = tcolor, linewidth = 0.1),  # Minor grid lines
  ) + expand_limits(y = 0) 
ggsave('spainplayersplot.png', plt, width = 6, height = 6, units = "in", scale = 1)
