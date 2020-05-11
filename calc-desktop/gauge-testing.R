suppressPackageStartupMessages({
  # Data import
  library(arrow)
  
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)

  # Visualizations
  library(DT)
  library(ggplot2)
  library(echarts4r)
})

# Testing

source('../calculator-internal/fn_server.R')
players_raw <- read_parquet('../calculator-internal/player_raw.pdata')
picks_raw <- read_parquet('../calculator-internal/picks_raw.pdata')

values <- gen_df_values(players_raw,picks_raw,
                        qb_type = '1QB',league_size = '12',draft_type = 'Normal',
                        value_factor = 235,rookie_optimism = 80,future_factor = 80,
                        c('Player',"Age","Value")
                          )

teamA_input <- c('Robert Woods, WR LAR','Austin Ekeler, RB LAC')

teamA_values <- values %>% 
  filter(Player %in% teamA_input) %>%
  arrange(desc(Value))

teamB_input <- c('Keenan Allen, WR LAC','Darren Waller, TE LVR')

teamB_values <- values %>% 
  filter(Player %in% teamB_input) %>%
  arrange(desc(Value))

trade_players <- tibble(team = c('Team A', 'Team B'),
                        players = list(teamA_values, teamB_values)) %>%
  unnest(players)



e_charts() %>%
  e_gauge(
    value = 25,
    name = 'Team B',
    min = -100,
    max = 100,
    axisLine = list(
      lineStyle = list(
        color = list(
          c(0.2, '#00441b'),
          c(0.3, '#1b7837'),
          c(0.4, '#5aae61'),
          c(0.5, '#a6dba0'),
          c(0.6, '#c2a5cf'),
          c(0.7, '#9970ab'),
          c(0.8, '#762a83'),
          c(1, '#40004b'))))
    
  ) %>%
  e_title("Trade Gauge")
