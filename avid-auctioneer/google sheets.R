library(tidyverse)
library(googlesheets4)
library(furrr)

library(RColorBrewer)

library(ggridges)

library(ggthemes)

#Load ggplot themes
#ggplot2::theme_set(ggthemes::theme_fivethirtyeight())

plan(multiprocess)

# ?read_sheet()

get_players <- function(team, position){
  
  range <- case_when(position == "QB" ~ "A9:F15",
                     position == "RB" ~ "A18:F38",
                     position == "WR" ~ "A41:F61",
                     position == "TE" ~ "A64:F74")
  
  read_sheet("https://docs.google.com/spreadsheets/d/1kyDqGYQ6kG9WXJsAm5N0664gitR-EuAfavXPMOlLX8k/edit#gid=1006311623",
             col_names = c("Player","Drop","Acquired","Salary","Class","Length"),
             sheet = team,
             range = range) %>%
    select(-Drop) %>%
    drop_na()
}

get_salary <- function(salary, year, contract_length){
  if_else(year > contract_length, 0, salary)
}

get_guarantee <- function(salary, year, class){
  case_when(year == 1 ~ salary,
            class == "A" ~ salary,
            class == "B" ~ 0.75*salary,
            class == "C" ~ 0.5*salary,
            class == "D" ~ 0.25*salary,
            class == "E" ~ 0,
            class == "R1" & year == 2 ~ 0.5*salary,
            class == "R1" & year > 2 ~ 0,
            class %in% c("R2", "R3") & year > 1 ~ 0)
}

Team <-  c("Greg","Verb","Jeremy","Austin","Zach","Matt","Joe","Alex","Fernando","Brett","Chuck","Ray Jay")
Position <- c("QB","RB","WR","TE")

df <- expand_grid(Team, Position) %>%
  mutate(players = future_map2(Team, Position, get_players, .progress = TRUE)) %>%
  unnest(cols=c(players)) 

df2 <- df %>%
  mutate(Salary2020 = future_pmap(list(Salary, 1, Length), get_salary),
         Salary2021 = future_pmap(list(Salary, 2, Length), get_salary),
         Salary2022 = future_pmap(list(Salary, 3, Length), get_salary),
         Salary2023 = future_pmap(list(Salary, 4, Length), get_salary),
         Salary2024 = future_pmap(list(Salary, 5, Length), get_salary),
         Guarantee2020 = future_pmap(list(Salary2020, 1, Class), get_guarantee),
         Guarantee2021 = future_pmap(list(Salary2021, 2, Class), get_guarantee),
         Guarantee2022 = future_pmap(list(Salary2022, 3, Class), get_guarantee),
         Guarantee2023 = future_pmap(list(Salary2023, 4, Class), get_guarantee),
         Guarantee2024 = future_pmap(list(Salary2024, 5, Class), get_guarantee)) %>%
  #select(-Salary) %>%
  pivot_longer(cols = Salary2020:Guarantee2024, names_to = "Year", values_to = "Value") %>%
  #rowwise() %>%
  mutate(Type = ifelse(grepl("Salary", Year), "Salary", "Guarantee"),
         Year = as.numeric(str_remove(Year, "Salary|Guarantee")),
         Value = as.numeric(Value))

temp <- df2 %>%
  #filter(Type == "Salary") %>%
  group_by(Year,Type, Position) %>%
  summarise(Total = sum(Value)) %>%
  ungroup()


temp %>%
  filter(Type == "Salary") %>%
  ggplot(aes(Year,Total, fill=Position)) +
  geom_area() + 
  geom_area(data = temp %>% filter(Type == "Guarantee"), alpha = 0.5) +
  scale_fill_brewer(palette="Spectral") +
  theme_gdocs()

df2 %>%
  group_by(Year, Type) %>%
  summarise(Total = sum(Value)) %>%
  ungroup() %>%
  ggplot(aes(Year,Total, color=Type)) +
  geom_line()
  
df %>%
  group_by(Team) %>%
  tally()

df %>%
  group_by(Length) %>%
  tally()

df %>%
  #filter(Class %in% c("A","B","C","D","E")) %>%
  mutate(Position = factor(Position, levels = c('QB', 'RB', 'WR', 'TE'))) %>%
  group_by(Position, Length, Class) %>%
  summarise(Total = n()) %>%
  ggplot(aes(Length, Total, fill = Class)) +
  geom_bar(position = "stack", stat = "identity")  +
  scale_fill_brewer(palette="Spectral") +
  labs(x = "Contract Length",
       y = "Contract Count",
       title = "Count of Contract Length by Class") +
  #facet_wrap(~Position) +
  theme_gdocs()

df %>%
  #filter(Class %in% c("A","B","C","D","E")) %>%
  mutate(Position = factor(Position, levels = c('QB', 'RB', 'WR', 'TE'))) %>%
  group_by(Position, Length, Class) %>%
  summarise(Total = n()) %>%
  ggplot(aes(Class, Total, fill = as.factor(Length))) +
  geom_bar(position = "stack", stat = "identity")  +
  scale_fill_brewer(palette="Spectral") +
  labs(x = "Contract Class",
       y = "Contract Count",
       title = "Count of Contract Class by Length",
       legend = "Length") +
  #facet_wrap(~Position) +
  theme_gdocs()


df %>%
  mutate(Position = factor(Position, levels = c('QB', 'RB', 'WR', 'TE'))) %>%
  #filter(Class %in% c("A","B","C","D","E")) %>%
  group_by(Position, Length, Class) %>%
  summarise(Total = sum(Salary)) %>%
  ggplot(aes(Length, Total, fill = Class)) +
  geom_bar(position = "stack", stat = "identity")  +
  scale_fill_brewer(palette="Spectral") +
  labs(x = "Contract Length",
       y = "Total Salary",
       title = "Sum of Salary by Contract and Position") +
  facet_wrap(~Position) +
  theme_gdocs()


  
df %>%
  ggplot(aes(x= Salary, y = as.factor(Length))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
  geom_density_ridges()

df %>%
  ggplot(aes(x= Salary, y = as.factor(Class), fill = Length)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
  geom_density_ridges()
  
  df %>%
    ggplot(aes(x= Salary, y = as.factor(Class), fill = Length)) +
    geom_density_ridges_gradient(scale = 3) +
    scale_fill_viridis_c()
  
  write_csv(df, "auctioneer_salaries.csv")
