library(rvest)
library(tidyverse)
library(ratelimitr)
library(DBI)
library(RSQLite)
library(janitor)
library(magrittr)

# Ratelimit read_html to 100 pages per minute
lim_readhtml<-limit_rate(read_html,rate(n = 100,period = 60))

teamIDs<-read.csv('teamIDs.csv') %>% 
  select(mfl,pfr_ugly)

get_pfrsnaps <- function(TEAMID, YEAR) {
  pfr_url <-
    paste0('https://www.pro-football-reference.com/teams/',TEAMID,'/',YEAR,'-snap-counts.htm') %>%
    lim_readhtml()
  
  pfrselect_names <- html_nodes(pfr_url, '.left:nth-child(1)')
  
  pfr_ids <-tibble(id_pfr = html_attr(pfrselect_names, 'data-append-csv'),
                   names = html_attr(pfrselect_names, 'csk'))
  
  pfr_table <- pfr_url %>%
    html_node('table') %>%
    html_table(fill = TRUE) %>%
    set_names(.[1, ]) %>%
    clean_names() %>%
    bind_cols(pfr_ids) %>%
    slice(-1) %>%
    rename(snaps_offense = num, snaps_offpct = pct,
           snaps_defense = num_2,snaps_defpct = pct_2,
           snaps_st = num_3,snaps_stpct = pct_3) %>%
    select(-x, -names)
  
  pfr_table
}

pfr_snaps<-expand_grid(year = 2012:2019,pfr_ugly = teamIDs$pfr_ugly) %>% 
  left_join(teamIDs,by = 'pfr_ugly') %>% 
  mutate(pfr_ugly = tolower(pfr_ugly),snaps = mapply(get_pfrsnaps,pfr_ugly,year,SIMPLIFY = FALSE)) %>% 
  unnest_wider(snaps) %>% 
  unnest(col = names(.)) %>% 
  select(-pfr_ugly)

conn_srv<-dbConnect(RSQLite::SQLite(),'dynastyprocess.sqlite')

dbRemoveTable(conn_srv,name = 'pfr_snaps')

dbWriteTable(conn_srv,pfr_snaps, name = 'pfr_snaps')

dbDisconnect(conn_srv)
