library(ffscrapr)
library(tidyverse)
library(here)

setwd(here())

conn <- mfl_connect(2020, 52290, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)

mfl_roster <- ffscrapr::ff_rosters(conn)

mfl_points <- tibble()

#Scrape whole year
for (i in 2018:2020){
  for (j in 1:16){
    mfl_points <- mfl_points %>% rbind(ffscrapr::ff_playerscores(conn, season = i, week = j))
  }
}

mfl_points_summ <- mfl_points %>% 
  group_by(player_id) %>% 
  summarise(ppg = mean(as.numeric(points), na.rm = TRUE),
            games = n()) %>% 
  ungroup()
