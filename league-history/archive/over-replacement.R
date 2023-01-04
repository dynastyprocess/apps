# whatsthis <- ffscrapr::ff_schedule(input_connection)
# whatsthis <- ffscrapr::ff_standings(input_connection)
whatsthis <- ffscrapr::ff_starters(input_connection)
roster_reqs <- ffscrapr::ff_starter_positions(input_connection)
player_scores <- ffscrapr::ff_scoringhistory(input_connection, season = 2022)
ffpros <- ffprojections::fantasypros_rankings_history

try_joining <- whatsthis %>% 
  mutate(season = 2022,
         merge_name = nflreadr::clean_player_names(player_name)) %>% 
  left_join(player_scores, by = c("player_name", "pos", "team", "week", "season"), na_matches = "never") %>% 
  left_join(ffpros, by = c("merge_name", "week", "season"))

optimize_a_week <- try_joining %>% 
  filter(week == 1, franchise_name == "sox05syd")