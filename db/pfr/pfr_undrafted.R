library(rvest)
library(tidyverse)
library(ratelimitr)

# Ratelimit read_html to 100 pages per minute
lim_readhtml<-limit_rate(read_html,rate(n = 100,period = 60))

# Set parameters of loop
# Setting to yearmin to 1999 then filtering out 1999 gets players who entered in 2000 or later while accurately showing career accomplishments
yearmin<-1999 
yearmax<-2019
offset<-0

teamIDs<-read.csv('teamIDs.csv')

pfr_undrafted<-data.frame()

while (!is.na(offset)) {
  
  pfr_url<- paste0('https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=combined&',
                   'year_min=',yearmin,
                   '&year_max=',yearmax,
                   '&season_start=1&season_end=-1&undrafted=Y&pos%5B%5D=qb&pos%5B%5D=rb&pos%5B%5D=wr&',
                   'pos%5B%5D=te&pos%5B%5D=e&pos%5B%5D=t&pos%5B%5D=g&pos%5B%5D=c&pos%5B%5D=ol&pos%5B%5D=dt',
                   '&pos%5B%5D=de&pos%5B%5D=dl&pos%5B%5D=ilb&pos%5B%5D=olb&pos%5B%5D=lb&pos%5B%5D=cb&pos%5B%5D=s',
                   '&pos%5B%5D=db&pos%5B%5D=k&pos%5B%5D=p&draft_year_min=1936&draft_year_max=2019&draft_slot_min=1',
                   '&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb',
                   '&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t',
                   '&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de',
                   '&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb',
                   '&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=av',
                   '&c1comp=gt&c5val=1.0&order_by=av',
                   '&offset=',offset) %>% 
    lim_readhtml()
  
  pfrselect_names<-html_nodes(pfr_url,'.left:nth-child(2)')
  
  pfr_ids<-tibble(id_pfr = html_attr(pfrselect_names,'data-append-csv'),
                  names = html_attr(pfrselect_names,'csk')
  ) %>% 
    filter(!is.na(names))
  
  pfr_table<-pfr_url %>% 
    html_node('table') %>% 
    html_table(fill=TRUE) %>% 
    set_names(.[1,]) %>% 
    slice(-1) %>% 
    filter(Rk!='Rk') %>% 
    bind_cols(pfr_ids) %>% 
    select(-names) %>% 
    select(player = Player,career_from = From, career_end = To, allpro = AP1, probowl = PB, 
           career_av = AV, games = G, games_started = GS,id_pfr) %>% 
    filter(career_from!=1999) %>% 
    mutate_at(vars(career_from,career_end,allpro,probowl,career_av,games,games_started),as.numeric)
  
  pfr_undrafted<-pfr_undrafted %>% 
    bind_rows(pfr_table)
  
  offset<-html_node(pfr_url,'.next') %>% 
    html_attr('href') %>% 
    str_extract('[0-9]+$')
  
}


  
  