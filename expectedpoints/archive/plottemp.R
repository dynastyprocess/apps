
library(nflfastR)
library(ggimage)
library(grid)
library(ggrepel)

setwd(here::here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% filter(Season >= 2018)

logos <- nflfastR::teams_colors_logos


season_data <- epdata %>%
  filter(Week <= 17) %>% 
  select(Season, Team, gsis_game_id, contains("team")) %>% 
  unique() %>% 
  group_by(Season,Team) %>% 
  summarise(games=n_distinct((gsis_game_id)),
            across(contains("team"), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  inner_join(logos,by=c("Team"="team_abbr"))

asp_ratio <- 1.618
aspect.ratio = 1/asp_ratio

epa_logos <- season_data %>% 
  filter(Season != 2020, Team %in% c('JAX','NYJ','CAR','BAL')) %>% 
  ggplot(aes(rush_td_team_x/rush_yd_team, rush_td_team/rush_yd_team, group = Team)) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio) +
  geom_text_repel(aes(label = Season),force = 10) +
  #geom_point(aes(color=as.factor(Team), size=as.factor(Season))) +
  geom_path() +
  geom_abline() +
  #coord_fixed(ratio = 1) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    aspect.ratio = 1/asp_ratio
    ) +
  theme_light() +
  annotation_custom(textGrob("Underperformed",x=0.95, y=0.1, hjust=1, vjust=0,
                             gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
  annotation_custom(textGrob("Overperformed",x=0.05, y=0.9, hjust=0, vjust=1,
                             gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15)))

asp_ratio <- 1.618
width <- 10

ggsave("logo_plot_asp.png", epa_logos, width = width, height = width/asp_ratio, dpi = "retina")
