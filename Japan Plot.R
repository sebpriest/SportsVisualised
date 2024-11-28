# libraries used
library(dplyr)
library(ggplot2)

# setting working directory
setwd("~/Dropbox/Nationalities Project")

# reading in data
nationalities <- read.csv('nationalities.csv', row.names = 1)

# filtering for japanese players and cleaning 
japan_players <- nationalities %>%
  filter(`Nation` %in% c("jp Japan")) %>%
  select( c(2,3,4,6,7)) %>%
  group_by(season) %>%
  summarise(NumPlayers = sum(NumPlayers),Min = sum(Min)) %>%
  mutate(season = paste(substring(season, 3, 5), substring(season, 8, 9), sep = '') )



# setting japan's colours
bcolor <- '#150295'
tcolor <-  '#cec7ff'


plt <- ggplot(japan_players, aes(x = season, y = NumPlayers, group = 1)) +
  geom_vline(xintercept = '22-23', color = tcolor, linetype = "dashed") +
  geom_line(color = "#d594a0", linewidth = 1.2) + annotate("text",label = "Mitoma's\n Breakout\n Season",
                                                           x = "21-22", y = 13.8, color = tcolor, 
                                                           family = "Avenir", size = 3.4) + 
  labs(title = "# of 🇯🇵 Players in Prem & EFL", x = "Season", y = "# Players", 
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
    panel.grid.major = element_line(color = tcolor, size = 0.1),    # Major grid lines
    panel.grid.minor = element_line(color = tcolor, size = 0.1),  # Minor grid lines
  ) + expand_limits(y = 0) 
ggsave('japanplayersplot.png', plt, width = 6, height = 6, units = "in", scale = 1)