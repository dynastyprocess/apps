suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(RSQLite)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  
  # Plotting
  library(ggimage)
  library(grid)
  library(ggrepel)
  library(nflfastR)
  
  library(ggbeeswarm)
  
  library(reactable)  
})

# Import Data -------------------------------------------------------------

setwd(here::here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% 
  filter(season >= 2011, week<=17)

thruWeek <- 6
remainingWeeks <- 16-thruWeek

# Prepare Data ------------------------------------------------------------

topn <- epdata %>%
  mutate(class = ifelse(Week<=thruWeek,'pre','post')) %>% 
  group_by(Season, Pos, gsis_id, class) %>%
  summarise(#mean_fp_diff = mean(total_fp_diff),
            mean_fp_x = mean(total_fp_x),
            games = n()) %>% 
  ungroup() %>%
  pivot_wider(names_from = class, values_from = c(mean_fp_x, games)) %>% 
  filter(games_pre >= 0.65*thruWeek & games_post >= remainingWeeks * .65) %>% 
  group_by(Season, Pos) %>%
  #slice_max(mean_fp_diff_pre, n=5) %>% 
  slice_max(mean_fp_x_pre, n=12) %>% 
  ungroup()
  
filterEP <- epdata %>% 
  inner_join(topn, by = c("Season","gsis_id","Pos")) %>%
  mutate(class = ifelse(Week<=thruWeek,'pre','post')) %>% 
  filter(Pos == "RB") %>%
  # arrange(Season, Week) %>% 
  # group_by(Season, gsis_id) %>% 
  # mutate(gameNum = row_number()) %>% 
  # ungroup() %>% 
  select(Season, Week, Team, Name, Pos, total_fp_x, total_fp, class) %>% 
  pivot_longer(cols = c(total_fp_x, total_fp), names_to = "Exp", values_to = "values") %>% 
  mutate(combo = paste0(class,Exp))

filterEP %>% 
  group_by(class, Exp) %>% 
  summarise(mean(values)) %>% 
  ungroup()

#Plotting Differences
# filterEPdiff <- epdata %>% 
#   inner_join(topn, by = c("Season","gsis_id","Pos")) %>%
#   mutate(class = ifelse(Week<=thruWeek,'pre','post')) %>% 
#   filter(Pos == "RB") %>%
#   select(Season, Week, Team, Name, Pos, total_fp_diff, class)


# Plot Data ---------------------------------------------------------------
filterEP %>% 
  ggplot(aes(x=Week, y=values, color = Exp)) +
  #geom_point() +
  geom_quasirandom(aes(group = combo)) +
  geom_smooth(method="lm", se=FALSE, aes(group=combo)) +
  scale_x_discrete(breaks = c(2,4,6,8,10,12,14,16), limits = c(1,17)) +
  theme_bw() +
  labs(title = "Top 5 Negative Regression Candidates Through 6 Weeks 2011-2019")

filterEP %>% 
  ggplot(aes(x=Week, y=values, color = Exp)) +
  geom_point() +
  #geom_quasirandom(aes(group = combo)) +
  geom_smooth(method="lm", se=FALSE, aes(group=combo)) +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16), limits = c(1,17)) +
  theme_bw() +
  labs(title = "Top 12 RBs by EP Through 6 Weeks 2011-2019")
  

# Data tables -------------------------------------------------------------
topn2020 <- epdata %>%
  mutate(class = ifelse(Week<=thruWeek,'pre','post')) %>% 
  group_by(Season, Pos, Name, class) %>%
  summarise(mean_fp_diff = mean(total_fp_diff),
            mean_rush_yd = mean(rush_yd),
            mean_rush_yd_x = mean(rush_yd_x),
            mean_rush_td = mean(rush_td),
            mean_rush_td_x = mean(rush_td_x),
            games = n()) %>% 
  ungroup() %>%
  pivot_wider(names_from = class, values_from = c(mean_fp_diff, mean_rush_yd, mean_rush_yd_x, mean_rush_td, mean_rush_td_x, games)) %>% 
  filter(games_pre >= 0.65*thruWeek & (games_post >= remainingWeeks * .65 | Season == 2020)) %>% 
  group_by(Season, Pos) %>%
  slice_max(mean_fp_diff_pre, n=5) %>% 
  ungroup() %>% 
  filter(Pos == "RB")


reactable(topn2020,
          columns = list(
            Season = colDef(format = colFormat(digits = 0))
          ),
          defaultColDef = colDef(format = colFormat(digits = 1)),
          defaultPageSize = 50)
