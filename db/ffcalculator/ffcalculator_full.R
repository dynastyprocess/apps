library(tidyverse)
library(janitor)
library(DBI)
library(jsonlite)
library(furrr)
library(magrittr)
library(glue)

get_adp <- function(adp_type,adp_year){
  
  adp_url <- glue::glue('https://fantasyfootballcalculator.com/api/v1/adp/{adp_type}?year={adp_year}')
  
  if(read_json(adp_url)$status != "Error"){
  
  read_json(adp_url)$players %>% 
    tibble() %>% 
    unnest_wider(1)
  }
  else {return(NA)}
  }

adp_types <- c('dynasty','rookie','ppr','2qb')

key <- tibble(ffcalculator = adp_types,
       saved_type = c('dynasty','rookies','redraft','redraft_2qb'))

years <- c(2010:2019)

aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")
team_ids <- dbGetQuery(aws_db,"SELECT mfl,ffcalc FROM dp_teamids")
dbDisconnect(aws_db)

insert_mergename <- . %>%
  mutate(merge_name = name) %>% 
  mutate_at('merge_name',str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

plan(multisession)

adp_data <- expand.grid(adp_type = adp_types,season = years) %>% 
  left_join(key,by = c('adp_type'='ffcalculator')) %>% 
  mutate(data = future_map2(adp_type,season,get_adp)) %>% 
  filter(!is.na(data)) %>% 
  unnest_wider(data) %>% 
  unnest(cols = c(player_id, name, position, team, adp, adp_formatted, times_drafted, 
                  high, low, stdev, bye)) %>% 
  insert_mergename() %>% 
  left_join(team_ids,by = c('team'='ffcalc')) %>% 
  mutate(team = mfl) %>% 
  select(season,adp_type,ffcalculator_id = player_id,
         name,merge_name,position,team,adp,times_drafted,high,low,stdev)

aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db") %T>% 
  dbWriteTable("ffcalculator_adp",adp_data,overwrite=TRUE) %>% 
  dbDisconnect()
