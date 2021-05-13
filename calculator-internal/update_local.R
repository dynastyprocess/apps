suppressPackageStartupMessages({
  library(arrow)
  library(tidyverse)
  library(httr)
  library(glue)
  # library(stringr)
})

update_local_values <- function(){
  
  on.exit({
    POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a/fail",
         body = "Failed to update calculator_values!")
  })
  
read.csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-players.csv") %>% 
  mutate(player = paste0(player,", ",pos," ",team)) %>% 
  write_parquet('../calculator-internal/player_raw.pdata')

read.csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-picks.csv") %>% 
  dplyr::filter(!is.na(pick)) %>% 
  write_parquet('../calculator-internal/picks_raw.pdata')

on.exit(NULL)

POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a",
     body = glue("Successfully updated local calculator data at {Sys.time()}"))
}

update_local_values()
