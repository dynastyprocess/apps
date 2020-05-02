suppressPackageStartupMessages({
  library(arrow)
  library(tidyverse)
  # library(stringr)
})

read.csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-players.csv") %>% 
  mutate(player = paste0(player,", ",pos," ",team)) %>% 
  write_parquet('player_raw.pdata')

read.csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-picks.csv") %>% 
  dplyr::filter(grepl('2020 Pick',player)) %>% 
  rownames_to_column(var = 'pick') %>% 
  mutate(pick=as.numeric(pick)) %>% 
  write_parquet('picks_raw.pdata')

message(paste0("Updated local calculator data as of ",Sys.time()))
