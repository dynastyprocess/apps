library(tidyverse)
library(arrow)
library(janitor)
library(lubridate)
library(hrbrthemes)
library(mgcv)

ranks <- read_parquet('sql/ranks.pdata')

cleaned_ranks <- ranks %>% 
  clean_names() %>% 
  transmute(rank_type = ifelse(instance_id == 80, 'rank_1qb','rank_2qb'),
       rank_date = as_date(snap_date),
       rank_year = year(rank_date),
       rank_month = month(rank_date),
       player_id,player_name,position,avg,
       ) %>% 
  pivot_wider(names_from = rank_type,values_from = avg) %>% 
  filter_all(~!is.na(.x)) %>% 
  arrange(rank_year,rank_month,rank_1qb)

cleaned_ranks %>% 
  # filter(rank_1qb<=150) %>% 
  ggplot(aes(rank_1qb,rank_2qb,color = position)) +
  # geom_point() + 
  scale_x_reverse() +
  scale_y_reverse()+
  theme_modern_rc() +
  geom_abline(slope = 1, intercept = 0,color = 'white',size = 2) +
  geom_smooth(method = 'gam')
  # geom_smooth(method = 'loess')
  # coord_cartesian(xlim = c(0,25),ylim = c(0,25))

model_gam <- function(data){
  gam(rank_2qb ~ s(rank_1qb,bs = "cs"),data = data)
  # gam(rank_2qb ~ rank_1qb, data = data, span = 0.75)
}

grouped <- cleaned_ranks %>% 
  group_by(position) %>% 
  nest() %>% 
  mutate(model = map(data,model_gam))
