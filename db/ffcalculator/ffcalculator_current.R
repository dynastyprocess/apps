suppressPackageStartupMessages({
library(tidyverse)
library(janitor)
library(DBI)
library(jsonlite)
library(glue)
library(magrittr)
library(httr)

  
  
get_adp <- function(adp_type,adp_year){
  
  adp_url <- glue::glue('https://fantasyfootballcalculator.com/api/v1/adp/{adp_type}?teams=12&year={adp_year}')
  
  get_output <- httr::GET(adp_url)
  
  if(get_output$status_code !=200){return(NA)} else {
  
  json_output <- content(get_output,as = "text") %>%
    parse_json()
  
  if(json_output$status != "Success") {return(NA)} else {
  
  json_output$players %>% 
    tibble() %>% 
    unnest_wider(1)

  }}}

})

insert_mergename <- . %>%
  mutate(merge_name = name) %>% 
  mutate_at(merge_name,str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at(merge_name,str_squish) %>%
  mutate_at(merge_name,tolower)

season <- c('2020')

adp_data <- tibble(scrape_date = Sys.Date(),
                   ffcalculator_types = c('dynasty','rookie','ppr','2qb'),
                   adp_types = c('dynasty','rookie','redraft','redraft_2qb'),
                   season = season) %>% 
  mutate(data = map2(ffcalculator_types,season,get_adp)) %>% 
  filter(!is.na(data)) %>%
  unnest_wider(data) %>% 
  unnest(cols = c(player_id, name, position, team, adp, adp_formatted, times_drafted, 
                  high, low, stdev, bye)) %>% 
  select(scrape_date,season,adp_type = adp_types,ffcalculator_id = player_id,
         name,position,team,adp,times_drafted,high,low,stdev) %>% 
  insert_mergename()


aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")

dbWriteTable(aws_db,"ffcalculator_adp_current",adp_data,overwrite=TRUE)

dbDisconnect(aws_db)

message("Scraped FFCalculator ADP on {Sys.time()!}")
