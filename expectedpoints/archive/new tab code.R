temp <- epdata %>%
  filter(gsis_name == "Lamar Jackson", season == "2019") %>%
  arrange(week_season_num) %>%
  select(gsis_id, gsis_name, team, gsis_pos, week_season, pass_fp, pass_fp_x) %>%
  group_by(gsis_id) %>%
  # mutate(Total = sum(.data[[input$selectVar]], na.rm = TRUE),
  #        Average = mean(.data[[input$selectVar]], na.rm = TRUE)) %>%
  pivot_longer(pass_fp:pass_fp_x, names_to = "metric", values_to = "value") %>%
  ungroup() %>%
  group_by(gsis_id, metric) %>%
  pivot_wider(names_from = week_season,
              values_from = value) %>%
  ungroup() %>%
  select(gsis_name, team, gsis_pos, contains("Week"), Total, Average) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  arrange(desc(Average))


temp2 <- epdata %>%
  filter(gsis_name == "Patrick Mahomes", season == "2019") %>%
  arrange(week_season_num) %>%
  select(gsis_id, gsis_name, team, gsis_pos, week, pass_fp, pass_fp_x) %>%
  group_by(gsis_id) %>%
  # mutate(Total = sum(.data[[input$selectVar]], na.rm = TRUE),
  #        Average = mean(.data[[input$selectVar]], na.rm = TRUE)) %>%
  pivot_longer(pass_fp:pass_fp_x, names_to = "metric", values_to = "value") %>%
  ungroup()

temp2 %>%
ggplot(aes(x = week, y = value, color = metric)) +
  geom_point(size = 3) +
  theme_bw() + 
  #labs(x="Week", y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
  theme(plot.title = element_text(face='bold'),
        panel.spacing = unit(0,"lines"),
        text = element_text(size=18)) +
  geom_smooth(method = "loess", na.rm = TRUE, fill = NA)

temp2 <- epdata %>%
filter(gsis_name == "Amari Cooper", season == "2019") %>%
  arrange(week_season_num) %>%
  select(gsis_id, gsis_name, team, gsis_pos, week, rec_fp, rec_fp_x) %>%
  group_by(gsis_id) %>%
  # mutate(Total = sum(.data[[input$selectVar]], na.rm = TRUE),
  #        Average = mean(.data[[input$selectVar]], na.rm = TRUE)) %>%
  pivot_longer(rec_fp:rec_fp_x, names_to = "metric", values_to = "value") %>%
  ungroup()

temp2 %>%
  ggplot(aes(x = week, y = value, color = metric)) +
  geom_point(size = 3) +
  theme_bw() + 
  #labs(x="Week", y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
  theme(plot.title = element_text(face='bold'),
        panel.spacing = unit(0,"lines"),
        text = element_text(size=18)) +
  geom_smooth(method = "loess", na.rm = TRUE, fill = NA)
