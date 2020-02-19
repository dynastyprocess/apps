library(rvest)
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(stringr)
library(furrr)
# library(RSQLite)
library(DBI)
library(janitor)

setwd(here())

teamIDs <- read.csv("teamIDs.csv",fileEncoding = 'UTF-8-BOM') # Hardcoded Team IDs ----

# Determine list of FP pages to scrape: ----

FP_pages<-read.csv('fantasypros/fantasypros-pages.csv',fileEncoding = 'UTF-8-BOM',na.strings = '')

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
           ECRType = case_when(page == 'dynasty-overall'~ 'do', # where d = dynasty, r = redraft, w = weekly, o = overall, p = positional
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
    mutate_at(vars(one_of('ECRType','Player','Pos','Team')),as.character) 
  return(df_ecr)
}


# Scrape, clean, and pivot ----

plan(multiprocess)

df_scrapelong<-FP_pages %>%
  select(FP_page = page,page_type = type) %>% 
  mutate(data = future_map(FP_page,scrape_FP)) %>% 
  hoist(data,
        'ECRType' = 'ECRType', 'Player'='Player', 'ID' = 'ID', 
        'Pos'='Pos', 'Team'='Team','ECR'='Avg',
        'SD' = 'Std Dev','Best' = 'Best', 'Worst' = 'Worst') %>%
  select(-data) %>% 
  unnest(cols=names(.)) %>% 
  mutate(scrapeDate = Sys.Date(),
         mergename = tolower(gsub('(-)|[[:punct:]]|( Jr)|( Sr)|( III)|( II)|( IV)','',Player))) %>% 
  nest_join(teamIDs,by=c('Team'='fp')) %>% # cleans up teamnames to the MFL standard, because #teamMFL
  hoist(teamIDs,'tm'='mfl') %>% 
  select(-teamIDs) %>% 
  filter(!(grepl("idp",page_type)&!(Pos %in% c('DL','LB','DB')))) %>%  # FILTER OUT if the page_type has the words idp in it AND the position is NOT in DL/LB/DB
  filter(!(grepl("-lb|lb-",FP_page)&Pos!='LB')&!(grepl("-dl|dl-",FP_page)&Pos!='DL')&!(grepl("-db|db-",FP_page)&Pos!='DB')) %>%   # FILTER OUT extra IDP position listings
  filter(!(grepl("-rb|rb-",FP_page)&Pos!='RB')&!(grepl("-wr|wr-",FP_page)&Pos!='WR')&!(grepl("-qb|qb-",FP_page)&Pos!='QB')&!(grepl("-te|te-",FP_page)&Pos!='TE')) %>% 
  clean_names()# FILTER OUT extra offense position listings

# Write to AWS DB ----
         
db_fp<-dbConnect(odbc::odbc(),'dynastyprocess_db')

dbWriteTable(db_fp,'fp_ecr',df_scrapelong,append = TRUE)

dbDisconnect(db_fp)

# # In case the WSIS/WSID box goes away, this code might be helpful later.
#
# df_name <- webpage %>%
#   html_node("table") %>%
#   html_nodes(".player-row") %>% 
#   tibble() %>% 
#   transmute(Player = html_text(html_node(.,'.full-name')),
#          Team = html_text(html_node(.,'.grey'))))