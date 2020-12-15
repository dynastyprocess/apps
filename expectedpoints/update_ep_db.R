# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(earth)
library(here)
library(arrow)
library(furrr)
library(vip)
library(slider)

library(nflfastR)
library(DBI)
library(here)
library(curl)

# set.seed(1234)
# memory.limit(size=300000000)
# doParallel::registerDoParallel()
# nflfastR::update_db()
# con <- dbConnect(RSQLite::SQLite(), "pbp_db")
# df <- dbSendQuery(con, "select * from nflfastR_pbp")
# df_fetch <- dbFetch(df)
# pbp <- df_fetch
# dbDisconnect(con)
# rosters2 <- read_csv("https://raw.githubusercontent.com/samhoppen/NFL_Positions/master/nfl_positions_2011_2020.csv")

# Raw Data ----------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")

rosters <- dbGetQuery(con, "select season, case when position in ('HB','FB') then 'RB' else position end as 'position',
                                   full_name, gsis_id, birth_date, sportradar_id
                            from nflfastr_rosters_current
                            where gsis_id is not NULL
                            union
                            select season, case when position in ('HB','FB') then 'RB' else position end as 'position',
                                   full_name, gsis_id, birth_date, sportradar_id
                            from nflfastr_rosters_historical
                            where gsis_id is not NULL")

# playerid <- dbGetQuery(con,
#                        "Select sportradar_id as sportradar_id3, gsis_id from dp_playerids
#                         where sportradar_id is not null and gsis_id is not null")

sportradar_cleaned <- rosters %>%
  filter(!is.na(sportradar_id), position %in% c("QB","WR","TE","RB")) %>% 
  select(gsis_id,
         sportradar_id2 = sportradar_id) %>% 
  unique()

rosters <- rosters %>% 
  left_join(sportradar_cleaned, by = "gsis_id") %>%
  #left_join(playerid, by = "gsis_id") %>% 
  mutate(sportradar_id = if_else(is.na(sportradar_id), sportradar_id2, sportradar_id)) %>% 
  select(-sportradar_id2)

pbp <- dbGetQuery(con, "select season, week, game_date, game_id, posteam, play_type, `desc`, yards_gained, rusher_player_id, two_point_conv_result, two_point_attempt, yardline_100, down, roof, temp, wind, rush_touchdown, fumble_lost, run_gap, run_location, rush_attempt, receiver_player_id, passer_player_id, sack, air_yards, play_id, qb_hit, pass_attempt, yards_after_catch, complete_pass, interception, pass_location, pass_touchdown, qb_scramble, shotgun, half_seconds_remaining, wp from nflfastr_pbp")

pbp <- nflfastR::decode_player_ids(pbp)

dbDisconnect(con)

setwd(here())

load(file = "new_models.rda")


# Functions ---------------------------------------------------------------
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

get_rate <- function(x,y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}

cpoe_func <- function(x,y){
  mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
}

# Rush Data ---------------------------------------------------------------
rushdf <- pbp %>%
  filter(play_type == "run",
         !grepl("kneel",desc),
         !is.na(yards_gained)) %>%   
  inner_join(dplyr::select(rosters,
                          season,
                          gsis_id,
                          rusher_sportradar_id = sportradar_id,
                          rusher_gsis_name = full_name,
                          rusher_gsis_bday = birth_date,
                          rusher_gsis_pos = position),
            by = c("rusher_player_id" = "gsis_id", "season")) %>%
  mutate(rusher_age = get_age(rusher_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         yards_gained = ifelse(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         down = ifelse(two_point_attempt == 1, 5, down),
         temp = case_when(roof == "closed" | roof == "dome" ~ 68,
                          is.na(temp) ~ 60,
                          TRUE ~ as.numeric(temp)),
         wind = case_when(roof == "closed" | roof == "dome" ~ 0,
                          is.na(wind) ~ 8,
                          TRUE ~ as.numeric(wind)),  
         rushFP = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         score = ifelse(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir),
         game_id = as.factor(game_id)) %>%
  
  arrange(rusher_player_id, game_id) %>%
  group_by(rusher_gsis_name, rusher_player_id) %>%
  mutate(ypc_ToDate = slide2_dbl(yards_gained, rush_attempt, ~get_rate(.x,.y), .before = Inf, .after = -1)) %>%
  ungroup()%>%
  
  cbind(predict(RushYDmod, new_data= .)) %>%
  rename(eRushYD = .pred) %>%
  cbind(predict(RushTDmod, new_data= ., type="prob")) %>%
  rename(eRushTD = .pred_1) %>%
  mutate(eRushTD = ifelse(eRushTD < 0, 0, eRushTD)) %>%
  cbind(predict(RushFPmod, new_data= .)) %>%
  rename(eRushFP = .pred) %>%
  select(season, week, posteam, game_id, rusher_player_id, rusher_sportradar_id, rusher_gsis_name, rusher_gsis_pos, rusher_age, 
         yards_gained, rushFP, rush_touchdown, two_point_converted, eRushYD, eRushTD, eRushFP)

# Pass Data ---------------------------------------------------------------
passdf <- pbp %>%
  filter(play_type == "pass",
         sack == 0) %>%  
  inner_join(dplyr::select(rosters,
                          season,
                          gsis_id,
                          receiver_sportradar_id = sportradar_id,
                          receiver_gsis_name = full_name,
                          receiver_gsis_bday = birth_date,
                          receiver_gsis_pos = position),
            by = c("receiver_player_id" = "gsis_id", "season")) %>%
  inner_join(dplyr::select(rosters,
                          season,
                          gsis_id,
                          passer_sportradar_id = sportradar_id,
                          passer_gsis_name = full_name,
                          passer_gsis_bday = birth_date,
                          passer_gsis_pos = position),
            by = c("passer_player_id" = "gsis_id", "season")) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         air_yards = ifelse(two_point_attempt == 1, yardline_100, air_yards),
         air_yards = ifelse(season < 2006, NA, air_yards),
         #yards_gained = ifelse(season < 2006, NA, yards_gained),
         down = ifelse(two_point_attempt == 1, 5, down),
         complete_pass = ifelse(two_point_attempt == 1 & grepl("is complete", desc), 1, complete_pass),
         yards_gained = ifelse(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         temp = case_when(roof == "closed" | roof == "dome" ~ 68,
                          is.na(temp) ~ 60,
                          TRUE ~ as.numeric(temp)),
         wind = case_when(roof == "closed" | roof == "dome" ~ 0,
                          is.na(wind) ~ 8,
                          TRUE ~ as.numeric(wind)),         
         recFP = 6*pass_touchdown + 2*two_point_converted  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         passFP =  4*pass_touchdown + 2*two_point_converted  + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         score = ifelse(pass_touchdown == 1 | two_point_converted == 1, 1, 0),
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         air_yards = ifelse(air_yards < -10, 0, air_yards),
         air_is_zero = ifelse(air_yards==0,1,0),
         abs_air_yards = abs(air_yards),
         targetline = yardline_100 - air_yards,
         game_id = as.factor(game_id)) %>%
  
  arrange(passer_player_id, game_id, play_id) %>%
  group_by(passer_gsis_name, passer_player_id) %>%
  mutate(CompRate_ToDate = slide2_dbl(complete_pass, pass_attempt, ~get_rate(.x,.y), .before = Inf, .after = -1))  %>%
  ungroup() %>%
  
  arrange(receiver_player_id, game_id, play_id) %>%
  group_by(receiver_gsis_name, receiver_player_id) %>%
  mutate(rAvgYAC_ToDate = slide2_dbl(yards_after_catch, pass_attempt, ~get_rate(.x,.y), .before = Inf, .after = -1))  %>%
  ungroup() %>%  
  
  cbind(predict(Recmod, new_data= ., type="prob")) %>%
  rename(eRec = .pred_1, neRec = .pred_0) %>%
  cbind(predict(RecYDmod, new_data= .)) %>%
  rename(eRecYDs = .pred) %>%
  cbind(predict(RecTDmod, new_data= ., type="prob")) %>%
  rename(eRecTDs = .pred_1, neRecTD = .pred_0) %>%
  mutate(eRecTDs = ifelse(eRecTDs < 0, 0, eRecTDs)) %>%
  cbind(predict(RecFPmod, new_data= .)) %>%
  rename(eRecFP = .pred) %>%
  cbind(predict(PassFPmod, new_data= .)) %>%
  rename(ePassFP = .pred) %>%
  select(season, week, posteam, game_id,
         receiver_player_id, receiver_sportradar_id, receiver_gsis_name, receiver_gsis_pos, receiver_age,
         passer_player_id, passer_sportradar_id, passer_gsis_name, passer_gsis_pos, passer_age,
         passFP, recFP, yards_gained, yards_after_catch, pass_touchdown, air_yards, complete_pass,
         eRec, eRecYD = eRecYDs, eRecTD = eRecTDs, eRecFP, ePassFP)

rm(pbp)

# Combine Rushing and Passing ---------------------------------------------
rushGame <- rushdf %>%
  group_by(season, posteam, week, game_id, rusher_player_id, rusher_sportradar_id, rusher_gsis_name, rusher_gsis_pos, rusher_age) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE),
            rush_att = n()) %>%
  ungroup() %>%
  select(season, posteam, week, game_id, rusher_player_id, rusher_sportradar_id, rusher_gsis_name, rusher_gsis_pos, rusher_age,
         rush_fp = rushFP, rush_yd = yards_gained, rush_att, rush_td = rush_touchdown,
         rush_yd_x = eRushYD, rush_td_x = eRushTD, rush_fp_x = eRushFP)

recGame <- passdf %>%
  group_by(season, posteam, week, game_id, receiver_player_id, receiver_sportradar_id,
           receiver_gsis_name, receiver_gsis_pos, receiver_age) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE),
            rec_tar = n()) %>%
  ungroup() %>%
  select(season, posteam, week, game_id, receiver_player_id, receiver_sportradar_id, receiver_gsis_name, receiver_gsis_pos, receiver_age,
         rec_fp = recFP, rec_yd = yards_gained, rec_td = pass_touchdown, rec_ay = air_yards, rec_tar, rec_yac = yards_after_catch,
         rec_comp = complete_pass, rec_comp_x = eRec, rec_yd_x = eRecYD, rec_fp_x = eRecFP, rec_td_x = eRecTD)

passGame <- passdf %>%
  group_by(season, posteam, week, game_id, passer_player_id, passer_sportradar_id, passer_gsis_name, passer_gsis_pos, passer_age) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE),
            pass_att = n()) %>%
  ungroup() %>%
  select(season, posteam, week, game_id, passer_player_id, passer_sportradar_id, passer_gsis_name, passer_gsis_pos, passer_age,
         pass_att, pass_fp = passFP, pass_yd = yards_gained, pass_td = pass_touchdown, pass_yac = yards_after_catch,
         pass_comp = complete_pass, pass_ay = air_yards, pass_comp_x = eRec, pass_yd_x = eRecYD, pass_td_x = eRecTD, pass_fp_x = ePassFP)

all_games <- 
  full_join(rushGame, recGame,  by=c("game_id", "season", "rusher_player_id" = "receiver_player_id", "week", "posteam")) %>%
  mutate(combo_id = ifelse(is.na(rusher_player_id), receiver_player_id, rusher_player_id),
         combo_sportradar_id = ifelse(is.na(rusher_sportradar_id), receiver_sportradar_id, rusher_sportradar_id),
         combo_name = ifelse(is.na(rusher_gsis_name), receiver_gsis_name, rusher_gsis_name),
         combo_pos = ifelse(is.na(rusher_gsis_pos), receiver_gsis_pos, rusher_gsis_pos),
         combo_age = ifelse(is.na(rusher_age), receiver_age, rusher_age)) %>%
  full_join(passGame, by=c("game_id", "season", "combo_id" = "passer_player_id", "week", "posteam")) %>%
  mutate(player_id = combo_id,
         sportradar_id = ifelse(is.na(combo_sportradar_id), passer_sportradar_id, combo_sportradar_id),
         player_age = ifelse(is.na(combo_age), passer_age, combo_age),
         gsis_name = ifelse(is.na(combo_name), passer_gsis_name, combo_name),
         gsis_pos = ifelse(is.na(combo_pos), passer_gsis_pos, combo_pos),
         proxy_pass_fp = ifelse(gsis_pos == "QB", pass_fp, rec_fp),
         proxy_pass_fp_x = ifelse(gsis_pos == "QB", pass_fp_x, rec_fp_x) ) %>%
  rowwise() %>%
  mutate(total_fp = sum(rush_fp, proxy_pass_fp, na.rm = TRUE),
         total_fp_x = sum(rush_fp_x, proxy_pass_fp_x, na.rm = TRUE),
         total_fp_diff = total_fp - total_fp_x,
         
         total_yd = sum(pass_yd, rush_yd, rec_yd, na.rm = TRUE),
         total_yd_x = sum(pass_yd_x, rush_yd_x, rec_yd_x, na.rm = TRUE),
         total_yd_diff = total_yd - total_yd_x,
         
         total_td = sum(pass_td, rush_td, rec_td, na.rm = TRUE),
         total_td_x = sum(pass_td_x, rush_td_x, rec_td_x, na.rm = TRUE),
         total_td_diff = total_td - total_td_x,
         
         rush_fp_diff = rush_fp - rush_fp_x,
         rush_yd_diff = rush_yd - rush_yd_x,
         rush_td_diff = rush_td - rush_td_x,
         
         rec_fp_diff = rec_fp - rec_fp_x,
         rec_yd_diff = rec_yd - rec_yd_x,
         rec_td_diff = rec_td - rec_td_x,
         rec_comp_diff = rec_comp - rec_comp_x,
         
         pass_fp_diff = pass_fp - pass_fp_x,
         pass_yd_diff = pass_yd - pass_yd_x,
         pass_td_diff = pass_td - pass_td_x,
         pass_comp_diff = pass_comp - pass_comp_x
         
         #season_num = as.numeric(as.character(season))
         
  ) %>%
  ungroup() %>%
  group_by(game_id, posteam) %>%
  mutate(
    rush_fp_team = sum(rush_fp, na.rm = TRUE),
    rush_yd_team = sum(rush_yd, na.rm = TRUE),
    rush_td_team = sum(rush_td, na.rm = TRUE),
    rush_att_team = sum(rush_att, na.rm = TRUE),
    rush_fp_team_x = sum(rush_fp_x, na.rm = TRUE),
    rush_yd_team_x = sum(rush_yd_x, na.rm = TRUE),
    rush_td_team_x = sum(rush_td_x, na.rm = TRUE),
    rush_fp_team_diff = sum(rush_fp_diff, na.rm = TRUE),
    rush_yd_team_diff = sum(rush_yd_diff, na.rm = TRUE),
    rush_td_team_diff = sum(rush_td_diff, na.rm = TRUE),  
    
    rec_fp_team = sum(rec_fp, na.rm = TRUE),
    rec_yd_team = sum(rec_yd, na.rm = TRUE),
    rec_td_team = sum(rec_td, na.rm = TRUE),
    rec_fp_team_x = sum(rec_fp_x, na.rm = TRUE),
    rec_yd_team_x = sum(rec_yd_x, na.rm = TRUE),
    rec_td_team_x = sum(rec_td_x, na.rm = TRUE),
    rec_fp_team_diff = sum(rec_fp_diff, na.rm = TRUE),
    rec_yd_team_diff = sum(rec_yd_diff, na.rm = TRUE),
    rec_td_team_diff = sum(rec_td_diff, na.rm = TRUE),
    
    pass_fp_team = sum(pass_fp, na.rm = TRUE),
    pass_yd_team = sum(pass_yd, na.rm = TRUE),
    pass_td_team = sum(pass_td, na.rm = TRUE),
    pass_ay_team = sum(pass_ay, na.rm = TRUE),
    pass_att_team = sum(pass_att, na.rm = TRUE),
    pass_comp_team = sum(pass_comp, na.rm = TRUE),
    pass_fp_team_x = sum(pass_fp_x, na.rm = TRUE),
    pass_yd_team_x = sum(pass_yd_x, na.rm = TRUE),
    pass_td_team_x = sum(pass_td_x, na.rm = TRUE),
    pass_fp_team_diff = sum(pass_fp_diff, na.rm = TRUE),
    pass_yd_team_diff = sum(pass_yd_diff, na.rm = TRUE),
    pass_td_team_diff = sum(pass_td_diff, na.rm = TRUE),
    
    total_fp_team = rush_fp_team + rec_fp_team,
    total_yd_team = rush_yd_team + rec_yd_team,
    total_td_team = rush_td_team + rec_td_team,
    total_fp_team_x = rush_fp_team_x + rec_fp_team_x,
    total_yd_team_x = rush_yd_team_x + rec_yd_team_x,
    total_td_team_x = rush_td_team_x + rec_td_team_x,
    total_fp_team_diff = rush_fp_team_diff + rec_fp_team_diff,
    total_yd_team_diff = rush_yd_team_diff + rec_yd_team_diff,
    total_td_team_diff = rush_td_team_diff + rec_td_team_diff
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(team = case_when(posteam == "ARZ" ~ "ARI",
                          posteam == "BLT" ~ "BAL",
                          posteam == "CLV" ~ "CLE",
                          posteam == "HST" ~ "HOU",
                          posteam == "JAC" ~ "JAX",
                          posteam == "LA" ~ "LAR",
                          posteam == "STL" ~ "LAR",
                          posteam == "SAN" ~ "LAC",
                          posteam == "SD" ~ "LAC",
                          posteam == "SL" ~ "LAR",
                          TRUE ~ posteam),
         week_season = paste0("Week ", week, ', ', season)) %>% 
  group_by(season, week, week_season) %>%
  mutate(week_season_num = cur_group_id()) %>%
  ungroup() %>%
  select(Season = season,
         Week = week,
         week_season,
         week_season_num,
         Team = team,
         gsis_game_id = game_id,
         gsis_id = player_id,
         sportradar_id,
         Name = gsis_name,
         Pos = gsis_pos,
         where(is.numeric)) %>% 
  select(-season, -week, -contains("proxy"), -contains("combo"), -rusher_age, -receiver_age, -passer_age)

write_parquet(all_games, "ep_1999_2019.pdata")

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")
# DBI::dbExecute(con, "truncate table dp_expectedpoints")
# DBI::dbAppendTable(con, "dp_expectedpoints", all_games)
DBI::dbRemoveTable(con,"dp_expectedpoints")
DBI::dbWriteTable(con, "dp_expectedpoints", all_games)
dbGetQuery(con, "select count(*) from dp_expectedpoints")
# temp <- dbGetQuery(con, "select * from dp_expectedpoints limit 50")
DBI::dbDisconnect(con)