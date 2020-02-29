suppressPackageStartupMessages({
library(rvest)
library(tidyverse)
library(janitor)
library(DBI)
library(here)
library(magrittr)
  
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd(here())
})

# suppressWarnings({team_ids <- read_csv("teamIDs.csv")})

aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")
team_ids <- dbGetQuery(aws_db,"SELECT mfl,otc FROM dp_teamids")
dbDisconnect(aws_db)

insert_mergename <- . %>%
  mutate(merge_name = player) %>% 
  mutate_at('merge_name',str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

url_contracts <- "https://overthecap.com/contracts/"

html_contracts <- read_html(url_contracts) %>% 
  html_node(".sortable")

team_id <- html_contracts %>% 
  html_nodes("tbody tr") %>% 
  html_attr("data-team") %>% 
  tibble(otc = .) %>% 
  left_join(team_ids,by = c('otc')) %>% 
  select(team = mfl)

player_id <- html_contracts %>% 
  html_nodes(":nth-child(1) a") %>% 
  html_attr('href') %>% 
  tibble(otc_url = .) %>% 
  filter(grepl("/player/",otc_url)) %>% 
  mutate(otc_id = str_match(otc_url,"[/]([0-9]+)[/]$")[,2])

contracts <- html_contracts %>% 
  html_table() %>% 
  clean_names() %>%
  select(-team) %>% 
  bind_cols(player_id) %>% 
  bind_cols(team_id) %>% 
  mutate_at(vars(total_value,avg_year,total_guaranteed,avg_guarantee_year,percent_guaranteed),parse_number) %>% 
  mutate_at(vars(total_value,avg_year,total_guaranteed,avg_guarantee_year),~./1000000) %>% 
  mutate_at(vars(total_value,avg_year,total_guaranteed,avg_guarantee_year),round,digits=2) %>% 
  mutate(percent_guaranteed = percent_guaranteed/100,
         scrape_date = Sys.Date()) %>% 
  insert_mergename() %>% 
  distinct(otc_id,.keep_all = TRUE)

aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")
  dbExecute(aws_db,"TRUNCATE TABLE overthecap_contracts")
  dbAppendTable(aws_db,"overthecap_contracts",contracts)
  dbDisconnect(aws_db)


message(glue::glue("Scraped OverTheCap contracts as of {Sys.time()}!"))
