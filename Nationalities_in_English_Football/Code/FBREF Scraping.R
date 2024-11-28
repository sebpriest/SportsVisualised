# required libraries
library(rvest)
library(dplyr)

# setting working directory
setwd("~/Dropbox/Nationalities Project")

# Download and combine loop
nationalities <- data.frame()
for (i in seq(20)){
  # Prem
  season <- paste(toString(seq(2025, 2000)[i+1]), toString(seq(2025, 2000)[i]), sep = "-")
  season_url <- paste("https://fbref.com/en/comps/9/", season, "/nations/", season, "-Premier-League-Nationalities", sep = "")
  destfilename <- paste('prem_url',season,'.html', sep = "")
  download.file(season_url, destfile = destfilename)
  nation_table <- read_html(destfilename) %>% html_element( "table.sortable") %>% html_table() 
  nation_table["season"] <- season
  nation_table["league"] <- 'Prem'
  Sys.sleep(1)
  nationalities <- rbind(nationalities, nation_table)
  
  # Champ
  season_url <- paste("https://fbref.com/en/comps/10/", season, "/nations/", season, "-Championship-Nationalities", sep = "")
  destfilename <- paste('cham_url',season,'.html', sep = "")
  download.file(season_url, destfile = destfilename)
  nation_table <- read_html(destfilename) %>% html_element( "table.sortable") %>% html_table() 
  nation_table["season"] <- season
  nation_table["league"] <- 'Cham'
  Sys.sleep(1)
  nationalities <- rbind(nationalities, nation_table)
  
  
  # League 1
  season_url <- paste("https://fbref.com/en/comps/15/", season, "/nations/", season, "-League-One-Nationalities", sep = "")
  destfilename <- paste('lge1_url',season,'.html', sep = "")
  download.file(season_url, destfile = destfilename)
  nation_table <- read_html(destfilename) %>% html_element( "table.sortable") %>% html_table() 
  nation_table["season"] <- season
  nation_table["league"] <- 'Lge1'
  Sys.sleep(1)
  nationalities <- rbind(nationalities, nation_table)
  
  
  # League 2
  season_url <- paste("https://fbref.com/en/comps/16/", season, "/nations/", season, "-League-Two-Nationalities", sep = "")
  destfilename <- paste('lge2_url',season,'.html', sep = "")
  download.file(season_url, destfile = destfilename)
  nation_table <- read_html(destfilename) %>% html_element( "table.sortable") %>% html_table() 
  nation_table["season"] <- season
  nation_table["league"] <- 'Lge2'
  Sys.sleep(1)
  nationalities <- rbind(nationalities, nation_table)
}

# cleaning
nationalities <- nationalities %>%
  filter(`Rk` != 'Rk') %>%
  mutate(
    `# Players` = as.numeric(`# Players`),
    `Min` = as.numeric(gsub(",", "", `Min`)),
    `Min` = ifelse(is.na(`Min`), 0, `Min`)
  ) %>%
  rename(NumPlayers = `# Players`)

# exporting as csv for more convenient use
write.csv(nationalities)
