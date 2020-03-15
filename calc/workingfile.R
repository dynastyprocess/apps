library(stringr)
library(lubridate)
library(glue)



rookie_optimism <- 80/100  
leaguesize <- 12 + 0.0001
futurerookie_factor <- 80/100
value_factor <- 0.0235

calculate_value <- function(df){
  df %>% 
    mutate(value = 10500 * exp(-value_factor * ecr))
} 

label_currentpicks <- function(df) {
  df %>% 
  mutate(
    season = year(as_date(update_date)),
    rookie_round = (pick %/% leaguesize)+1,
    round_pick = round(pick %% leaguesize,0),
    pick_label = glue("{season} Pick {as.character(rookie_round)}.{str_sub(paste0(0,round_pick),-2,-1)}")
  )
  }

add_futurepicks <- function(df,fr_f,l_s){
  
  n1 <- df %>%
    mutate(season = season + 1,
           rookie_round = case_when(rookie_round == 1 ~ '1st',
                                    rookie_round == 2 ~ '2nd',
                                    rookie_round == 3 ~ '3rd',
                                    rookie_round >= 4 ~ paste0(rookie_round,'th')),
           eml = case_when(round_pick <= l_s/3 ~ 'Early',
                           round_pick <= l_s*2/3 ~ 'Mid',
                           TRUE ~ 'Late'))
  
  n1_eml <- n1 %>% 
    group_by(season,rookie_round,eml) %>% 
    summarise(value = mean(value)*fr_f) %>% 
    ungroup() %>% 
    mutate(pick_label = paste(season,eml,rookie_round))
  
  n1_summary <- n1 %>% 
    group_by(season,rookie_round) %>% 
    summarise(value = mean(value)*fr_f) %>% 
    ungroup() %>% 
    mutate(pick_label = paste(season,rookie_round))
  
  n2_summary <- n1_summary %>% 
    mutate(season = season + 1,
           value = value*fr_f,
           pick_label = paste(season,rookie_round))

  df %>% 
    mutate(rookie_round = as.character(rookie_round)) %>% 
    bind_rows(n1_eml,n1_summary,n2_summary) %>% 
    mutate(position = "PICK") %>% 
    arrange(desc(value))
}

pickvalues <- rookies_raw %>% 
  mutate(ecr = rookie_optimism*high_model + (1-rookie_optimism)*low_model) %>% 
  label_currentpicks() %>% 
  calculate_value() %>% 
  add_futurepicks(futurerookie_factor,leaguesize) %>% 
  select(pick_label,position,value)
