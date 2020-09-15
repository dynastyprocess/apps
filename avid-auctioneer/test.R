library(ffscrapr)
library(tidyverse)
library(here)

setwd(here())

conn <- mfl_connect(2020, 52290, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)

get_playerscore <- function(year,week){
  mfl_getendpoint(conn, "playerScores", W=week, YEAR=year, RULES=1) %>% 
    pluck("content","playerScores","playerScore") %>% 
    tibble() %>% 
    unnest_wider(1)
}

nested_ppg <- crossing(year = 2018:2019,
               week = 1:1) %>%
  mutate(playerscore = map2(year,week,get_playerscore))

mfl_ppg <- nested_ppg %>%
  unnest(cols = c(playerscore)) %>%
  group_by(year, id) %>%
  summarise(ppg = round(mean(as.numeric(score), na.rm = TRUE),2),
            games = n())
  # pivot_wider(names_from = year,
  #             values_from = c(ppg, games))

mfl_salaries <- mfl_getendpoint(conn, "salaries") %>%
  pluck("content","salaries","leagueUnit","player") %>% 
  tibble() %>% 
  unnest_wider(1)

mfl_players <- mfl_getendpoint(conn, "players", DETAILS=1) %>%
  pluck("content","players","player") %>% 
  tibble() %>% 
  unnest_wider(1)

mfl_join <- mfl_salaries %>%
  left_join(mfl_ppg, by = "id") %>%
  left_join(mfl_players, by = "id") %>%
  filter(!is.na(name)) %>%
  separate(name, c("last","first"), sep = ",") %>%
  mutate(draft_year = as.numeric(draft_year),
         salary = as.numeric(salary),
         full_name = str_trim(paste(first, last),"both"),
         ppg = ifelse(games >= 6, ppg, NA)) %>%
  arrange(year, position, -ppg) %>%
  group_by(position, year) %>%
  mutate(ppg_rank = seq_along(ppg),
         ppg_rank = ifelse(is.na(ppg), NA, ppg_rank)) %>%
  ungroup() %>%
  select(full_name, position, draft_year, salary, ppg_year = year, contains("ppg"), contains("games"))

write_csv(mfl_join, "auctioneer_salaries.csv")

# df2 <- crossing(year = 2018:2019,
#                 week = c("AVG","YTD")) %>% 
#   mutate(playerscore = map2(year,week,get_playerscore))
# 
# df2temp <- df2 %>%
#   unnest(cols = c(playerscore)) %>%
#   filter(week == "AVG")
# 
# joindf <- temp %>%
#   left_join(df2temp, by = c("year","id"))
