library(rvest)
library(tidyverse)
library(ratelimitr)
library(magrittr)
library(RSQLite)
library(DBI)
library(janitor)

lim_readhtml<-limit_rate(read_html,rate(n = 100,period = 60)) # Ratelimit read_html to 100 pages per minute

pfr_combine<-function(yearid){
  
  pfr_url<- paste0('https://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1',
                   '&year_min=',yearid,'&year_max=',yearid,'&pos%5B%5D=QB&pos%5B%5D=WR&pos%5B%5D=TE&pos%5B%5D=RB&pos%5B%5D=FB&show=all&order_by=year_id') %>% 
    lim_readhtml()
  
  pfrselect_names<-html_nodes(pfr_url,'.left:nth-child(3)')
  
  pfr_ids<-tibble(id_pfr = html_attr(pfrselect_names,'data-append-csv'),
                  names = html_attr(pfrselect_names,'csk')
  ) %>% 
    slice(-1)
  
  pfrselect_cfbid<-html_nodes(pfr_url,'.right:nth-child(8)') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    str_remove('https://www.sports-reference.com/cfb/players/') %>% 
    str_remove('.html')
  
  pfr_table<-pfr_url %>% 
    html_node('table') %>% 
    html_table(fill=TRUE) %>% 
    bind_cols(pfr_ids) %>% 
    select(-names) %>% 
    filter(Rk!='Rk')
  
  pfr_output<-pfr_table %>% 
    filter(College == 'College Stats') %>% 
    bind_cols(id_cfbref = pfrselect_cfbid) %>% 
    bind_rows({pfr_table %>% filter(College!= 'College Stats')}) %>%
    mutate_at(vars(Year,Rk),as.numeric) %>% 
    arrange(Year,Rk)
  
  
pfr_output
  
}

df_combine<-tibble(year = lapply(c(2000:2019),pfr_combine)) %>% 
  unnest_wider(year) %>% 
  unnest(cols = names(.)) %>% 
  select(-Rk,-College) %>%
  separate(`Drafted (tm/rnd/yr)`,sep = '/',into = c('draft_team','draft_round','draft_pick','draft_year')) %>% 
  mutate_all(str_squish) %>% 
  mutate_at(vars(Age,AV,Wt,`40YD`,Vertical,BenchReps,`Broad Jump`,`3Cone`,Shuttle),as.numeric) %>% 
  clean_names() %>% 
  rename(forty = x40yd,cone = x3cone) %>% 
  mutate(scrape_date = Sys.Date())

conn_srv<-dbConnect(RSQLite::SQLite(),'dynastyprocess.sqlite')

dbRemoveTable(conn_srv,name = 'pfr_combine')

dbWriteTable(conn_srv,df_combine, name = 'pfr_combine')

dbDisconnect(conn_srv)

