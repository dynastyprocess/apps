suppressPackageStartupMessages({
library(tidyverse)
library(janitor)
library(DBI)
library(jsonlite)
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
})

season <- 2020

adp_data <- tibble(scrape_date = Sys.Date(),
                   ffcalculator_types = c('dynasty','rookie','ppr','2qb'),
                   adp_type = c('dynasty','rookie','redraft','redraft_2qb'),
                   season = season) %>% 
  mutate(data = map2(ffcalculator_types,season,get_adp)) %>% 
  filter(!is.na(data)) %>%
  unnest_wider(data) %>% 
  unnest(cols = c(player_id, name, position, team, adp, adp_formatted, times_drafted, 
                  high, low, stdev, bye)) %>% 
  select(scrape_date,season,adp_type,ffcalculator_id = player_id,name,position,team,adp,times_drafted,high,low,stdev)


aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")

dbWriteTable(aws_db,"ffcalculator_adp_current",adp_data,overwrite=TRUE)

dbDisconnect(aws_db)

message("Scraped FFCalculator ADP on {Sys.time()!}")
