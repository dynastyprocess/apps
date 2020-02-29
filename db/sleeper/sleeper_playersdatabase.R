suppressPackageStartupMessages({
  
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(jsonlite)
  library(glue)
  library(httr)
  
})

insert_mergename <- . %>%
  mutate(merge_name = full_name) %>% 
  mutate_at(merge_name,str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at(merge_name,str_squish) %>%
  mutate_at(merge_name,tolower)

sleeper <- GET('https://api.sleeper.app/v1/players/nfl') %>% 
  content(as = 'text') %>% 
  parse_json() %>% 
  tibble() %>% 
  unnest_wider(1)

sleeper_players <- sleeper %>% 
  select(sleeper_id = player_id,fantasy_data_id,gsis_id,full_name,
         position,team,age,college,sportradar_id,stats_id,espn_id,
         yahoo_id,rotoworld_id,rotowire_id) %>% 
  mutate(scrape_date = Sys.Date()) %>% 
  insert_mergename()

dbConnect(odbc::odbc(),"dynastyprocess_db") %T>% 
  dbWriteTable("sleeper_players",sleeper_players,overwrite=TRUE) %>% 
  dbDisconnect()

message(glue("Scraped Sleeper's players database at {Sys.time()} successfully!"))