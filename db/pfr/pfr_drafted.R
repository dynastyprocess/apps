library(rvest)
library(tidyverse)
library(ratelimitr)
library(DBI)

# Ratelimit read_html to 100 pages per minute
lim_readhtml<-limit_rate(read_html,rate(n = 100,period = 60))

# Set parameters of loop
yearmin<-2000
yearmax<-2019
offset<-0

aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")
team_ids <- dbGetQuery(aws_db,"SELECT mfl,pfr FROM dp_teamids")
dbDisconnect(aws_db)

pfr_drafted<-data.frame()

while (!is.na(offset)) {
  # Construct scraper loop
  
  pfr_url<-glue('https://www.pro-football-reference.com/play-index/draft-finder.cgi?request=1&year_min={}
                  yearmin}&year_max={yearmax}&show=all&order_by=default&offset={offset}') %>%
    lim_readhtml()
  
  # extract pfr ids from html attribute
  pfrselect_names<-html_nodes(pfr_url,'.left:nth-child(5)') 
  
  pfr_ids<-tibble(id_pfr = html_attr(pfrselect_names,'data-append-csv'),
                  names = html_attr(pfrselect_names,'csk')
                  ) %>% 
    filter(!is.na(names))
  
  pfr_table<-pfr_url %>% 
    html_node('table') %>% 
    html_table(fill=TRUE) %>% 
    set_names(.[1,]) %>% 
    rename(CollegeStats=18) %>% 
    slice(-1) %>% 
    filter(Rk!='Rk') %>% 
    mutate_at(vars(Rk,Year,Rnd,Pick,DrAge,From,To,AP1,PB,St,CarAV,G,GS),as.numeric)
  
  # extract CFB reference ids
  
  pfrselect_cfbid<-html_nodes(pfr_url,'.right:nth-child(18)') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    str_remove('https://www.sports-reference.com/cfb/players/') %>% 
    str_remove('.html')
  
  # Combine and clean
  
  pfr_output<-pfr_table %>% 
    filter(CollegeStats =='College Stats') %>% 
    bind_cols(id_cfbref = pfrselect_cfbid) %>% 
    bind_rows({pfr_table %>% filter(CollegeStats!='College Stats')}) %>% 
    arrange(Rk) %>% 
    bind_cols(pfr_ids) %>% 
    left_join(teamIDs,by = c('Tm'='pfr')) %>% 
    select(draft_year = Year,draft_round = Rnd,draft_pick = Pick,player = Player,team=mfl,pos = Pos,
           draft_age = DrAge,career_from = From, career_end = To, allpro = AP1, probowl = PB, 
           years_starter = St, career_av = CarAV, games = G, games_started = GS, college = `College/Univ`,
           id_cfbref,id_pfr)
  
  pfr_drafted<-pfr_drafted %>% 
    bind_rows(pfr_output)
  
  # get next if available
  offset<-html_node(pfr_url,'.next') %>% 
    html_attr('href') %>% 
    str_extract('[0-9]+$')
  
}

conn_srv<-dbConnect(odbc::odbc(),'dynastyprocess_db')

# dbExecute(conn_srv,"TRUNCATE TABLE pfr_drafted")

dbAppendTable(conn_srv,pfr_drafted, name = 'pfr_drafted')

dbDisconnect(conn_srv)
