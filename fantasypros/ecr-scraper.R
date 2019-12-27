library(rvest)
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(stringr)
library(RSQLite)
library(DBI)

setwd(here())

teamIDs <- read.csv("teamIDs.csv",fileEncoding = 'UTF-8-BOM') # Hardcoded Team IDs ----

# Determine list of FP pages to scrape: ----

FP_pages<-read.csv('fantasypros-pages.csv',fileEncoding = 'UTF-8-BOM',na.strings = '')

current_timeframe<-case_when(
  between(month(today()),10,12) | (month(today())==9 & day(today())>=10) ~ '3_inseason',
  month(today())>=8 & day(today()) >=15 ~ '2_preseason',
  TRUE ~ '1_offseason'
  )

FP_pages <- FP_pages %>% 
  filter(timeframe == current_timeframe)

# Scraping Function ----

scrape_FP<-function(page){

  webpage<- paste0('https://www.fantasypros.com/nfl/rankings/',page,'.php') %>% 
    read_html() %>% 
    html_node('table')

  df_nodes <- webpage %>%
    html_nodes(".player-row") %>% 
    html_nodes('.hide-print') %>%
    html_nodes('input')
  
  
  df_name<-tibble(Player = html_attr(df_nodes,'data-name'),
                   ID = html_attr(df_nodes,'data-id'),
                   Team = html_attr(df_nodes,'data-team'),
                   Pos = html_attr(df_nodes,'data-position'))
  
  if(!nrow(df_name)>0){return(tibble())} # error-catching, hopefully!
  
  df_rank <- webpage %>%
    html_table(fill=TRUE) %>%
    .[4:min(10,ncol(.))] %>%  # hardcoded getting exactly ten columns, looks like the df somehow has ~90 blank columns
    filter(!(Avg=="" | Avg == '&nbsp' | grepl('googletag',Avg))) %>% 
    select(one_of('Age','Best','Worst','ECR','Avg','SD'='Std Dev'))
  
  df_ecr<-bind_cols(df_name,df_rank) %>%
    mutate(Pos = str_sub(Pos,1,2),
           Pos = case_when(str_detect(Pos,"^K[0-9]*$")~'PK',
                           str_detect(Pos,"^S[0-9]*$")~'DB',
                           Pos == 'DS' ~ 'DST',
                           Pos == 'CB' ~ 'DB',
                           Pos == 'DE' ~ 'DL',
                           Pos == 'DT' ~ 'DL',
                           TRUE ~ Pos
                           ),
           Page = case_when(page == 'dynasty-overall'~ 'do', # where d = dynasty, r = redraft, w = weekly, o = overall, p = positional
                            page %in% c('ppr-cheatsheets','ros-ppr-overall',
                                        'idp-cheatsheets','ros-idp') ~ 'ro',
                            page %in% c('ppr-flex','idp') ~ 'wo',
                            grepl('dynasty',page) ~ 'dp',
                            grepl('cheatsheets',page)~'rp',
                            grepl('ros',page)~'rp',
                            page == 'rookies' ~'rookies',
                            TRUE ~ 'wp'
                            )
           ) %>% 
    mutate_at(vars(one_of('Age','Best','Worst','ECR','SD','Avg','Std Dev')),as.numeric) %>% 
    mutate_at(vars(one_of('Page','Player','Pos','Team')),as.character) 
  return(df_ecr)
}

vScrapeFP<-Vectorize(scrape_FP,SIMPLIFY = FALSE)

# Actual Scrape ----

df_scrapelong<-FP_pages %>%
  select(page) %>% 
  mutate(data = vScrapeFP(page)) %>% 
  hoist(data,
        'Page' = 'Page', 'Player'='Player', 'ID' = 'ID', 
        'Pos'='Pos', 'Team'='Team','ECR'='Avg',
        'SD' = 'Std Dev','Best' = 'Best', 'Worst' = 'Worst') %>%
  select(-data,-page) %>% 
  unnest(cols=names(.)) %>% 
  mutate(scrapeDate = Sys.Date())

df_scrapewide<-df_scrapelong %>% 
  pivot_longer(c('ECR','SD','Best','Worst')) %>% 
  filter(!is.na(Player)) %>% 
  unite(col = 'Temp',Page,name,sep='') %>% 
  pivot_wider(names_from = 'Temp',values_from = 'value',values_fn=list(value=min)) %>% 
  mutate(scrapeDate = Sys.Date())

db_fp<-dbConnect(RSQLite::SQLite(),'fantasypros.sqlite')
dbWriteTable(db_fp,'fp-wide',df_scrapewide)
dbWriteTable(db_fp,'fp-long',df_scrapelong)
dbDisconnect(db_fp)

dbListFields(db_fp,'fp-wide')

# # In case the WSIS/WSID box goes away, this code might be helpful later.
#
# df_name <- webpage %>%
#   html_node("table") %>%
#   html_nodes(".player-row") %>% 
#   tibble() %>% 
#   transmute(Player = html_text(html_node(.,'.full-name')),
#          Team = html_text(html_node(.,'.grey'))))

