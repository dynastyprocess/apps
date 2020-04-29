suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-players.csv") %>% 
  mutate(player = paste0(player,", ",pos," ",team)) %>% 
  write_parquet('player_raw.pdata')

read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-picks.csv") %>% 
  filter(str_detect(player,'2020 Pick')) %>% 
  rownames_to_column(var = 'pick') %>% 
  mutate(pick=as.numeric(pick)) %>% 
  write_parquet('picks_raw.pdata')

message(paste0("Updated local calculator data as of ",Sys.Date()))