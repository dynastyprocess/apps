# SETUP  ----------------------------------------------------------

suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
  library(ffsimulator)
  library(nflreadr)
  library(tidyverse)
  library(gt)
  library(gtExtras)
  library(RColorBrewer)
  library(emoji)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  
})

platform <- "sleeper"
league_id <- 652718526494253056

platform <- "espn"
league_id <- 805175

conn <- sleeper_connect(
  season = 2021,
  league_id = 739986523688230912,
  rate_limit_number = 1000,
  rate_limit_seconds = 60
)

load_conn <- function(platform, league_id){
  ff_connect(platform = platform, league_id = league_id, season = 2021)
}

get_year_range <- function(conn){
  ff_league(conn)
  
  ff_league(conn) %>% mutate(range = str_split(years_active, "-"),
                             range = map(range, as.numeric)) %>% view()
  
}

#Change for each week
scoring_histoy <- ff_scoringhistory(conn, season = 2021) %>%
  filter(week <= 16, pos %in% c("QB","RB","WR","TE")) %>% 
  group_by(season, sleeper_id, pos, gsis_id) %>% 
  summarise(ppg = mean(points),
            games = n()) %>%
  ungroup() %>% 
  group_by(season, pos) %>% 
  mutate(ppg = if_else(games < 5, NA_real_, ppg),
         ppg_rank = row_number(-ppg),
         ppg_rank_value = 1000 * exp(-0.1 * ppg_rank),
         games_played_percent = games/13) %>% 
  ungroup() %>% 
  filter(!is.na(sleeper_id)) %>% 
  select(season, sleeper_id, ppg_rank, ppg_rank_value, games, games_played_percent)

get_transaction_history <- 
  function(season) {
    conn <- sleeper_connect(
      season = 2021,
      league_id = 739986523688230912,
      rate_limit_number = 1000,
      rate_limit_seconds = 60
    )
    
    df <- 
      ff_transactions(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

get_draft_history <- 
  function(season) {
    conn <- sleeper_connect(
      season = 2021,
      league_id = 739986523688230912,
      rate_limit_number = 1000,
      rate_limit_seconds = 60
    )
    
    df <- 
      ff_draft(conn) %>% 
      mutate(overall = row_number()) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

get_standings_history <- 
  function(season) {
    conn <- sleeper_connect(
      season = 2021,
      league_id = 739986523688230912,
      rate_limit_number = 1000,
      rate_limit_seconds = 60
    )
    
    df <- 
      ff_standings(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

get_headshot <- 
  function(player_id, season){
    nflfastR::fast_scraper_roster(season) %>% 
      filter(sleeper_id == player_id) %>% 
      pull(headshot_url)
    
  }

# Draft Skill
draft_history <- 
  tibble(season = c(2021)) %>% 
  mutate(nested_draft = map(season, get_draft_history)) %>% 
  select(-season) %>% 
  unnest(cols = nested_draft) %>% 
  group_by(season, pos) %>% 
  mutate(pos_rank = row_number(overall),
         pos_rank_value = 1000 * exp(-0.1 * pos_rank)) %>% 
  ungroup()

draft_history_join <- 
  draft_history %>% 
  mutate(player_id = as.character(player_id),
         season = as.double(season)) %>% 
  left_join(scoring_histoy, by = c("season", "player_id" = "sleeper_id")) %>%
  mutate(value_diff = ppg_rank_value - pos_rank_value,
         games_played_percent = replace_na(games_played_percent, 0),
         injury_value_lost = pos_rank_value * (1-games_played_percent)) %>% 
  filter(pos %in% c("QB","RB","WR","TE"))

# draft_expectation_by_season <- 
#   draft_history_join %>% 
#   group_by(user_name, season) %>% 
#   summarise(pos_rank_value = sum(pos_rank_value, na.rm = TRUE),
#             ppg_rank_value = sum(ppg_rank_value, na.rm = TRUE),
#             injury_value_lost = sum(injury_value_lost, na.rm = TRUE)) %>% 
#   ungroup()

draft_values <- 
  draft_history_join %>% 
  group_by(user_name) %>% 
  summarise(injury_value_oe = sum(injury_value_lost, na.rm = TRUE) / sum(pos_rank_value, na.rm = TRUE),
            draft_value_oe= sum(ppg_rank_value, na.rm = TRUE) / sum(pos_rank_value, na.rm = TRUE) - 1) %>% 
  ungroup()

#Determine best and worst picks
combine_word <- function(name, finish_string){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{finish_string}</span></div>"
  )
}

library(glue)
best_draft_picks <-
  draft_history_join %>%
  group_by(user_name) %>%
  slice_min(order_by = -value_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(best_string = glue("Drafted {pos}{pos_rank} - Current {pos}{ppg_rank}"),
         best_headshot = map2(player_id, season, get_headshot),
         player_name = glue("{player_name} ({season})"),
         best_combo = combine_word(player_name, best_string),
         best_combo = map(best_combo, gt::html)) %>%
  select(user_name, best_combo, best_headshot)

worst_draft_picks <-
  draft_history_join %>%
  group_by(user_name) %>%
  slice_min(order_by = value_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(worst_string = glue("Drafted {pos}{pos_rank} - Current {pos}{ppg_rank}"),
         worst_headshot = map2(player_id, season, get_headshot),
         player_name = glue("{player_name} ({season})"),
         worst_combo = combine_word(player_name, worst_string),
         worst_combo = map(worst_combo, gt::html)) %>%
  select(user_name, worst_combo, worst_headshot)

top_draft_teams <-
  draft_history_join %>%
  group_by(user_name, team) %>%
  summarise(avg_value = sum(pos_rank_value)) %>%
  ungroup() %>%
  group_by(user_name) %>%
  arrange(-avg_value) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(team = case_when(team == "NEP" ~ "NE",
                          team == "GBP" ~ "GB",
                          team == "KCC" ~ "KC",
                          team == "NOS" ~ "NO",
                          TRUE ~ team)) %>%
  left_join(select(nflfastR::teams_colors_logos, team_abbr, team_logo_espn = team_logo_wikipedia), by = c("team" = "team_abbr")) %>%
  select(user_name, team_logo_espn)

#Draft History Plot
draft_pick <- 
  draft_history %>% 
  group_by(season, user_name) %>% 
  summarise(yearly_pick = min(overall)) %>% 
  ungroup() %>%
  mutate(pick_suffix = case_when(yearly_pick %in% c(11,12,13) ~ "th",
                                 yearly_pick %% 10 == 1 ~ 'st',
                                 yearly_pick %% 10 == 2 ~ 'nd',
                                 yearly_pick %% 10 == 3 ~'rd',
                                 TRUE ~ "th")) %>%
  group_by(user_name) %>% 
  summarise(pick_year = paste(yearly_pick, pick_suffix, sep = "", collapse = ", "),
            years = n(),
            last_season = max(season)) %>% 
  ungroup()

# Records    
standings_history <-
  tibble(season = c(2021)) %>%
  mutate(nested_records = map(season, get_standings_history)) %>% 
  unnest(cols = nested_records) %>% 
  mutate(league_rank = row_number(-h2h_winpct)) %>% 
  mutate(finish_emoji = case_when(league_rank == 1 ~ emoji("trophy"),
                                  league_rank == 2 ~ medal("silver"),
                                  league_rank == 3 ~ medal("bronze"),
                                  league_rank %in% c(4,5,6) ~ emoji("flexed_biceps"),
                                  league_rank == 12 ~ emoji("taco"),
                                  TRUE ~ emoji("wastebasket"))) %>% 
  group_by(user_name) %>% 
  summarise(across(.cols = c(h2h_wins, h2h_losses, h2h_ties, allplay_wins, allplay_losses),
                   .fns = sum),
            emoji_collase = paste(finish_emoji, sep = "", collapse = ""),
            h2h_winpct = h2h_wins / (h2h_wins + h2h_losses + h2h_ties),
            allplay_winpct = allplay_wins / (allplay_wins + allplay_losses),
            first_place = sum(league_rank == 1),
            luck_pct = h2h_winpct - allplay_winpct) %>% 
  ungroup() %>% 
  select(user_name, h2h_winpct, allplay_winpct, emoji_collase, luck_pct, first_place)

# Transaction Tables Only 2 years? maybe more I forgot
# transaction_history <- 
#   tibble(season = c(2019:2020)) %>% 
#   mutate(nested_draft = map(season, get_transaction_history)) %>% 
#   unnest(cols = nested_draft)
# 
# trade_history <- 
#   transaction_history %>% 
#   filter(type_desc == "traded_for") %>% 
#   group_by(user_name) %>% 
#   summarise(trades = n_distinct(timestamp)) %>% 
#   ungroup()

# PLOTS
flea_df <- draft_pick %>% 
  filter(last_season == 2021) %>%
  inner_join(standings_history, by = "user_name") %>% 
  inner_join(best_draft_picks, by = "user_name") %>%
  inner_join(worst_draft_picks, by = "user_name") %>%
  inner_join(top_draft_teams, by = "user_name") %>%
  inner_join(draft_values, by = "user_name") %>% 
  mutate(user_name = str_to_title(user_name)) %>% 
  arrange(-h2h_winpct) %>% 
  # arrange(-first_place, -allplay_winpct) %>% 
  select(-c(first_place, last_season, pick_year))

saveRDS(flea_df, "data/flea_df_2021.RDS")



