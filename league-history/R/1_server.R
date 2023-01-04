
# FUNCTIONS ---------------------------------------------------------------

.sleeper_history <- function(league_endpoint) {
  history <- tibble::tibble(season = character(),
                            league_id = character())
  history <- history %>% 
    add_row(season = league_endpoint$season,
            league_id = league_endpoint$league_id)

  prev <- league_endpoint$previous_league_id
  
  while (!is.null(prev) && prev != "0") {
    prev_endpoint <- glue::glue("league/{prev}") %>%
      sleeper_getendpoint() %>%
      purrr::pluck("content")
    
    history <- history %>% 
      add_row(season = prev_endpoint$season,
              league_id = prev)
    
    prev <- prev_endpoint$previous_league_id
  }
  
  return(history)
}

get_standings_history <- function(season,
                                  league_id,
                                  platform) {
  
  local_connection <- ffscrapr::ff_connect(
    platform = platform,
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  df <- ff_standings(local_connection) %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(df)
}

get_draft_history <- function(league_id,
                              season){
  local_connection <- ffscrapr::ff_connect(
    platform = "sleeper",
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  df <- ff_draft(local_connection) %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(df)
}

get_transaction_history <- function(league_id,
                                    season){
  
  local_connection <- ffscrapr::ff_connect(
    platform = "sleeper",
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  df <- ff_transactions(local_connection) %>% 
    mutate(franchise_id = as.numeric(franchise_id)) %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(df)
}

create_league_id_history <- function(platform,
                                     league_id,
                                     conn){
  
  if (platform == "sleeper") {
    league_endpoint <- glue::glue("league/{conn$league_id}") %>%
      sleeper_getendpoint() %>%
      purrr::pluck("content")
    
    league_id_history <- .sleeper_history(league_endpoint = league_endpoint)
  } else {
    year_range <- ff_league(conn) %>%
      separate(col = years_active,
               into = c("min_year", "max_year"),
               sep = "-")
    
    league_id_history <- crossing(season = as.numeric(year_range$min_year):as.numeric(year_range$max_year),
                                  league_id)
  }
  
  league_id_history <- 
    league_id_history %>% 
    mutate(platform = platform,
           season = as.numeric(season)) %>% 
    arrange(season)
    
  return(league_id_history)
}

get_sleeper_playoffs <- function(league_id, season){
  
  local_connection <- ffscrapr::ff_connect(
    platform = "sleeper",
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )

  winners_query <- glue::glue('league/{league_id}/winners_bracket')
  
  sleeper_playoffs <- sleeper_getendpoint(winners_query) %>% 
    purrr::pluck("content") %>% 
    dplyr::bind_rows()
  
  playoffs_long <- 
    sleeper_playoffs %>% 
    pivot_longer(cols = c(w, l),
                 values_to = "franchise_id") %>% 
    mutate(playoff_outcome = case_when(p == 1 & name == "w" ~ 1,
                                       p == 1 & name == "l" ~ 2,
                                       p == 3 & name == "w" ~ 3,
                                       p == 3 & name == "l" ~ 4,
                                       p == 5 & name == "w" ~ 5,
                                       p == 5 & name == "l" ~ 6,
                                       TRUE ~ 99
    ),
    bye_wins = if_else(r == 2 & t1 == franchise_id, 1, 0),
    wins = if_else(name == "w", 1, 0),
    losses = if_else(name == "l", 1, 0))
  
  losers_query <- glue::glue('league/{league_id}/losers_bracket')
  
  sleeper_consolation <- sleeper_getendpoint(losers_query) %>% 
    purrr::pluck("content") %>% 
    dplyr::bind_rows()
  
  consolation_long <- 
    sleeper_consolation %>% 
    pivot_longer(cols = c(w, l),
                 values_to = "franchise_id") %>% 
    mutate(playoff_outcome = case_when(p == 1 & name == "w" ~ 7,
                                       p == 1 & name == "l" ~ 8,
                                       p == 3 & name == "w" ~ 9,
                                       p == 3 & name == "l" ~ 10,
                                       p == 5 & name == "w" ~ 11,
                                       p == 5 & name == "l" ~ 12,
                                       TRUE ~ 99),
    bye_wins = 0,
    wins = 0,
    losses = 0)
  
  playoffs_summary <- playoffs_long %>%
    bind_rows(consolation_long) %>% 
    group_by(franchise_id) %>% 
    summarise(playoff_h2h_wins = sum(wins) + sum(bye_wins),
              playoff_h2h_losses = sum(losses),
              final_place = min(playoff_outcome)) %>% 
    ungroup() %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(playoffs_summary)
  
}

# PULL DATA ---------------------------------------------------------------

# platform <- "sleeper"
# league_id <- 786958282513362944

# platform <- "mfl"
# league_id <- 54040

# input_connection <- ffscrapr::ff_connect(
#   platform = platform,
#   league_id = league_id,
#   season = 2022,
#   rate_limit_number = 1000,
#   rate_limit_seconds = 60
# )

get_league_history <- function(platform,
                               league_id,
                               conn){
  
  league_id_history <- create_league_id_history(platform = platform,
                                                league_id = league_id,
                                                conn = conn)
  
  standings_nested <- league_id_history %>% 
    mutate(yearly_standings = pmap(.l = list(season = season,
                                             league_id = league_id,
                                             platform = platform),
                                   .f = get_standings_history))
  
  standings_unnested <- standings_nested %>% 
    unnest(yearly_standings)
  
  
  playoffs_nested <- league_id_history %>% 
    mutate(playoff_standings = map2(.x = league_id,
                                    .y = season,
                                    .f = get_sleeper_playoffs))
  
  playoffs_unnested <- playoffs_nested %>% 
    unnest(playoff_standings)
  
  standings_history_combined <- standings_unnested %>% 
    left_join(playoffs_unnested, by = c("franchise_id", "user_name", "season", "league_id", "platform"))
  
  blank_seasons <- 
    crossing(user_name = unique(standings_history_combined$user_name),
             season = unique(standings_history_combined$season))
  
  pick_values <- 
    dp_values() %>% 
    filter(pos == "PICK",
           str_detect(player, "\\.")) %>% 
    transmute(pick_number = row_number(),
              value_1qb)
  
  draft_history_unnested <- league_id_history %>% 
    mutate(draft_history = map2(.x = league_id,
                                .y = season,
                                .f = get_draft_history)) %>% 
    select(-season) %>% 
    unnest(draft_history) %>% 
    filter(type == "linear") %>% 
    mutate(season = as.numeric(season)) %>% 
    group_by(season) %>% 
    mutate(pick_number = row_number()) %>% 
    ungroup() %>% 
    left_join(pick_values, by = "pick_number")
  
  draft_history_summary <-
    blank_seasons %>% 
    full_join(draft_history_unnested, by = c("user_name", "season")) %>%
    group_by(user_name, season) %>%
    summarise(first_round_picks = sum(if_else(is.na(pick) | round != 1, 0, 1)),
              total_picks = sum(if_else(is.na(pick), 0, 1)),
              total_pick_value = sum(value_1qb, na.rm = TRUE)) %>% 
    ungroup() %>%
    group_by(user_name) %>% 
    summarise(first_round_picks = sum(first_round_picks),
              picks_by_season = list(total_picks),
              total_picks = sum(total_picks),
              value_by_season = list(total_pick_value)) %>% 
    ungroup()
  
  transactions_unnested <- league_id_history %>% 
    mutate(transaction_history = map2(.x = league_id,
                                      .y = season,
                                      .f = get_transaction_history)) %>%
    unnest(transaction_history)
  
  transaction_summary <- 
    blank_seasons %>% 
    full_join(transactions_unnested, by = c("user_name", "season")) %>%
    group_by(user_name, season) %>%
    summarise(trade_count = n_distinct(if_else(type == "trade", timestamp, NaN)) - 1,
              players_added = sum(if_else(type %in% c("free_agent", "waiver_complete"), 1, 0))) %>% 
    ungroup() %>%
    group_by(user_name) %>% 
    summarise(trade_count = sum(trade_count),
              players_added_by_season = list(players_added),
              players_added = sum(players_added)) %>% 
    ungroup()
  
  standings_history <- 
    blank_seasons %>% 
    full_join(standings_history_combined, by = c("user_name", "season")) %>%
    mutate(finish_emoji = case_when(is.na(final_place) ~ emoji("black_square_button"),
                                    final_place == 1 ~ emoji("trophy"),
                                    final_place == 2 ~ medal("silver"),
                                    final_place == 3 ~ medal("bronze"),
                                    final_place %in% c(4,5,6) ~ emoji("check_mark"),
                                    final_place == 12 ~ emoji("taco"),
                                    TRUE ~ emoji("x"))) %>% 
    group_by(user_name) %>% 
    summarise(across(.cols = c(h2h_wins, h2h_losses, h2h_ties, allplay_wins, allplay_losses,
                               points_for, potential_points,
                               playoff_h2h_wins, playoff_h2h_losses),
                     .fns = ~sum(.x, na.rm = TRUE)),
              emoji_collase = paste(finish_emoji, sep = "", collapse = ""),
              h2h_winpct = h2h_wins / (h2h_wins + h2h_losses + h2h_ties),
              allplay_winpct = allplay_wins / (allplay_wins + allplay_losses),
              first_place = sum(final_place == 1, na.rm = TRUE),
              optimal_start_percent = points_for / potential_points,
              luck_pct = h2h_winpct - allplay_winpct,
              playoff_h2h_winpct = playoff_h2h_wins / (playoff_h2h_wins + playoff_h2h_losses)) %>% 
    mutate(playoff_h2h_winpct = replace_na(playoff_h2h_winpct, 0)) %>% 
    ungroup() %>% 
    left_join(transaction_summary, by = "user_name") %>% 
    left_join(draft_history_summary, by = "user_name") %>%
    
    arrange(-first_place, -allplay_wins, -allplay_winpct) %>% 
    
    transmute(user_name,
              optimal_start_percent,
              regular_season_record = glue::glue("{h2h_wins}-{h2h_losses}-{h2h_ties}"),
              h2h_winpct,
              allplay_winpct,
              emoji_collase, 
              luck_pct,
              playoff_record = glue::glue("{playoff_h2h_wins}-{playoff_h2h_losses}"),
              playoff_h2h_winpct,
              first_round_picks,
              total_picks,
              value_by_season,
              trade_count,
              players_added,
              players_added_by_season)
  waiter_hide()
  return(standings_history)
  
}



