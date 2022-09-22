# SETUP  ----------------------------------------------------------

suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
  library(ffsimulator)
  library(nflreadr)
  library(tidyverse)
  library(gt)
  library(gtExtras)
  # library(RColorBrewer)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  
})

platform <- "sleeper"
league_id <- 652718526494253056

# platform <- "mfl"
# league_id <- 22627

conn_obj <- load_conn(platform, league_id)
# rank_obj <- load_rankings(conn_obj)
# roster_obj <- load_rosters(platform, league_id, conn_obj)
roster_combo <- combine_sources(conn_obj, platform)
roster_combo %>% filter(franchise_name %in% c("Free Agents", "sox05syd")) %>% view()


team_name_fn <- function(var) {
  stringr::str_replace_all(
    var,
    c(
      "JAC" = "JAX",
      "STL" = "LA",
      "SL" = "LA",
      "ARZ" = "ARI",
      "BLT" = "BAL",
      "CLV" = "CLE",
      "HST" = "HOU",
      "SD" = "LAC",
      "OAK" = "LV"
    )
  )
}

load_conn <- function(platform, league_id){
  ff_connect(platform = platform, league_id = league_id, season = 2021)
}

get_ros_ranks <- function(){
  fp_rankings(page = "ros-ppr-overall", sport = "nfl", year = 2021) %>%
    transmute(fantasypros_id, ros_ecr = round(ecr,1))
}

get_projections_page <- function(pos){
  fp_projections(page = pos, sport = "nfl", year = 2021)
}


get_projections <- function(){
  tibble(position = c("qb","rb","wr","te")) %>% 
    mutate(projections = map(position, get_projections_page),
           position = str_to_upper(position)) %>% 
    unnest(projections) %>% 
    pivot_longer(cols = where(is.numeric))
}

load_projections <- function(platform, conn_obj){
  
  # Pull Scoring Rules
  if (platform == "fleaflicker") {
    league_scoring <- ff_scoring(conn_obj) %>% transmute(pos, points, event = as.character(event_id))
  } else {league_scoring <- ff_scoring(conn_obj)}
  
  #Clean scoring rules
  stat_mapping <-
    ffscrapr::nflfastr_stat_mapping %>%
    mutate(stat_name = case_when(nflfastr_event == "passing_yards" ~ "passing_yds",
                                 nflfastr_event == "completions" ~ "passing_cmp",
                                 nflfastr_event == "attempts" ~ "passing_att",
                                 nflfastr_event == "interceptions" ~ "passing_ints",
                                 nflfastr_event == "carries" ~ "rushing_att",
                                 nflfastr_event == "rushing_yards" ~ "rushing_yds",
                                 str_detect(nflfastr_event, "fumbles_lost")  ~ "misc_fl",
                                 nflfastr_event == "receptions" ~ "receiving_rec",
                                 nflfastr_event == "receiving_yards" ~ "receiving_yds",
                                 TRUE ~ nflfastr_event)) %>% 
    filter(platform == platform) %>% 
    select(-nflfastr_event) %>% 
    distinct()
  
  #Apply rules to each roster
  get_projections() %>% 
    inner_join(stat_mapping, by = c("name"="stat_name"), na_matches ="never") %>% 
    left_join(league_scoring, by = c("ff_event"="event", "position"="pos"), na_matches ="never") %>% 
    mutate(projected_points = value*points) %>% 
    group_by(position, fantasypros_id, player_name, team) %>% 
    summarise(projected_points = sum(projected_points, na.rm = TRUE))
  
}

load_rankings <- function(conn_obj){
  
  league_settings <- ff_league(conn_obj)
  
  if(league_settings$qb_type == "2QB/SF" & str_detect(league_settings$scoring_flags,"1_ppr")) {
    fp_rankings(page = "ppr-superflex", sport = "nfl")
  } else if (league_settings$qb_type == "2QB/SF") {
    fp_rankings(page = "superflex", sport = "nfl")
  } else if (str_detect(league_settings$scoring_flags,"1_ppr")) {
    fp_rankings(page = "ppr-flex", sport = "nfl") %>% 
      bind_rows(fp_rankings(page = "qb", sport = "nfl"))
  } else {fp_rankings(page = "flex", sport = "nfl") %>% 
      bind_rows(fp_rankings(page = "qb", sport = "nfl"))}
  
}

injury_data <- function(){
  nflreadr::load_injuries(2021) %>%
    filter(week == 4) %>% 
    left_join(select(dp_playerids(), gsis_id, fantasypros_id), by = "gsis_id") %>% 
    transmute(fantasypros_id,
              practice_status = case_when(practice_status == "Did Not Participate In Practice" ~ "DNP",
                                          practice_status == "Full Participation in Practice" ~ "Full",
                                          practice_status == "Limited Participation in Practice" ~ "Limited",
                                          TRUE ~ practice_status),
              report_status)
}

# snap_data <- function(){
#   snap_weeks <- 
#     load_snap_counts(2020:2021) %>%
#     left_join(select(load_schedules(), game_id, week))
#   
#   player_expand <- 
#     expand_grid(pfr_id = snap_weeks %>% pull(pfr_id) %>% unique(),
#                 week = c(1:17))
#     
#   player_expand %>% 
#     left_join(snap_weeks, by = c("pfr_id","week")) %>% 
#     mutate(offense_pct = replace_na(offense_pct, 0)) %>% 
#     group_by(pfr_id) %>% 
#     slice_tail(n = 16) %>% 
#     summarise(snap_data = list(offense_pct), .groups = "drop")
# }

load_expected_points <- function(){
  arrow::read_parquet("~/Documents/DynastyProcess/research/expected-points/expected_points_2021.pdata") %>% 
    select(
      player_name = full_name,
      # season,
      week,
      pos = position,
      total_fantasy_points_exp,
      total_fantasy_points,
      total_fantasy_points_diff,
      # offense_snaps,
      # offense_pct,
      gsis_id = player_id
    ) %>%
    filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
    group_by(gsis_id) %>%
    summarise(
      across(#.cols = contains("fantasy_points"),
        .cols = c(total_fantasy_points_exp,
                  total_fantasy_points,
                  total_fantasy_points_diff,
                  # offense_snaps,
                  # offense_pct
                  ),
             .fns = ~ mean(.x, na.rm = TRUE) %>% round(1)),
      # snap_data = list(offense_pct),
      games = n()
    ) %>%
    ungroup()
}

combine_sources <- function(conn_obj, platform){
  
  ffs_rosters(conn_obj) %>%
    full_join(load_projections(platform, conn_obj),
              by = c("fantasypros_id", "pos" = "position"),
              suffix = c(".rosters", ".proj")) %>% 
    full_join(load_rankings(conn_obj),
              by = c("fantasypros_id", "pos"),
              suffix = c("", ".ranks")) %>%
    left_join(injury_data(), by = "fantasypros_id", na_matches ="never") %>%
    left_join(select(dp_playerids(), fantasypros_id, gsis_id), by = "fantasypros_id") %>%
    left_join(get_ros_ranks(), by = "fantasypros_id") %>% 
    left_join(load_expected_points(), by = "gsis_id", na_matches ="never") %>% 
    
    transmute(franchise_name = replace_na(franchise_name, "Free Agents"),
              player_name = coalesce(player_name.rosters, player_name.proj, player_name),
              player_image_url,
              # player_id,
              # fantasypros_id,
              # sportradar_id = coalesce(sportradar_id, sportradar_id.ranks),
              position = factor(pos, levels = c("QB","RB","WR","TE"), ordered = TRUE),
              team = coalesce(team.rosters, team.proj, team),
              team = team_name_fn(team),
              ros_ecr,
              ovr_rank = rank,
              pos_rank = as.numeric(str_extract(pos_rank, "\\d+")),
              ecr,
              best,
              worst,
              roster_pct = player_owned_yahoo / 100,
              roster_pct = replace_na(roster_pct, 0),
              projected_points = replace_na(projected_points, 0),
              practice_status = replace_na(practice_status, ""),
              report_status = replace_na(report_status, ""),
              total_fantasy_points_exp = replace_na(total_fantasy_points_exp, 0),
              total_fantasy_points = replace_na(total_fantasy_points, 0),
              total_fantasy_points_diff = replace_na(total_fantasy_points_diff, 0),
              # offense_snaps,
              # offense_pct = offense_pct / 100
              # snap_data = case_when(is.na(snap_data) ~ list(rep(0,2)),
              #                       length(snap_data) < 2 ~ list(c(0,snap_data)),
              #                       TRUE ~ snap_data)
              # snap_data
              # snap_data = replace_na(snap_data, list(rep(0,2)))
              ) %>% 
    left_join(select(load_teams(), team_abbr, team_wordmark), by = c("team"="team_abbr"))
  
}



