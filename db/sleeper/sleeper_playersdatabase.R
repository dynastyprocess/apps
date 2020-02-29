suppressPackageStartupMessages({
  
  library(DBI)
  library(dplyr)
  library(magrittr)
  library(jsonlite)
  library(glue)
  
})

sleeper <- read_json('https://api.sleeper.app/v1/players/nfl') %>% 
  tibble() %>% 
  unnest_wider(1)

sleeper_players <- sleeper %>% 
  select(sleeper_id = player_id,fantasy_data_id,gsis_id,full_name,
         position,team,age,college,sportradar_id,stats_id,espn_id,
         yahoo_id,rotoworld_id,rotowire_id) %>% 
  mutate(scrape_date = Sys.Date())

dbConnect(odbc::odbc(),"dynastyprocess_db") %T>% 
  dbWriteTable("sleeper_players",sleeper_players,overwrite=TRUE) %>% 
  dbDisconnect()

message(glue("Scraped Sleeper's players database at {Sys.time()} successfully!"))