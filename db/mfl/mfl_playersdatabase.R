suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  # library(furrr)
  library(janitor)
  library(DBI)
  library(odbc)
  library(lubridate)
  
  options(stringsAsFactors=FALSE)
  options(scipen = 999)
})



year <- 2020

read_playerdb <- function(url) {
  df <- read_json(url)$players$player %>%
    tibble() %>%
    unnest_wider(1) %>% 
    filter(!(grepl("^TM|Coach|Off|Def|ST",position)))
  
  df
}

players <- tibble(season = year,
                  url = paste0("https://api.myfantasyleague.com/",season,
                               "/export?TYPE=players&L=&APIKEY=&DETAILS=1&SINCE=&PLAYERS=&JSON=1"),
                  data = map(url,read_playerdb)) %>% 
  unnest_wider(data) %>% 
  unnest(cols = c(position, name, id, team, draft_year, draft_team, stats_global_id, 
                  twitter_username, stats_id, cbs_id, fleaflicker_id, college, 
                  height, rotowire_id, jersey, weight, draft_round, draft_pick, 
                  birthdate, rotoworld_id, sportsdata_id, nfl_id, espn_id, 
                  status)) %>% 
  mutate(birthdate = as.numeric(birthdate),
         birthdate = as_datetime(birthdate),
         birthdate = as_date(birthdate)) %>% 
  rename(mfl_id = id) %>% 
  select(-url,-status)

aws_db <- dbConnect(odbc(),"dynastyprocess_db")

dbWriteTable(aws_db,'mfl_players_current',players,overwrite=TRUE)

dbDisconnect(aws_db)

message(glue::glue("Scraped MFL players database as of {Sys.time()}!"))
