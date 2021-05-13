library(tidyverse)
library(googlesheets4)
library(rvest)
library(stringr)
library(ffscrapr)
library(furrr)

#Get Values from KTC
ktc <- 
  read_html("https://keeptradecut.com/dynasty-rankings?page=0&filters=QB|WR|RB|TE&format=2") %>% 
  html_node("#rankings-page-rankings")

ktc_values <-
  tibble(Player = html_nodes(ktc,'a') %>% html_text(),
         KTC_Value = as.numeric(html_nodes(ktc,'.value p') %>% html_text())) %>% 
  mutate(Player = dp_cleannames(Player, lowercase = TRUE))

#Creat Functions to scrape Google Sheets
get_players <- function(team, position){
  Sys.sleep(10)
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

get_salary <- function(salary, year, contract_length, class){
  case_when(
    year > contract_length ~ 0,
    year == 1 ~ salary,
    class %in% c("A","R1","R2","R3") ~ salary,
    class == "B" ~ salary*(1.05)^(year-1),
    class == "C" ~ salary*(1.1)^(year-1),
    class == "D" ~ salary*(1.15)^(year-1),
    class == "E" ~ salary*(1.20)^(year-1),
    class == "R1" & year == 2 ~ 0.5*salary,
    TRUE ~ salary 
  )
  
  # if_else(year > contract_length, 0, salary)
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

# Team <-  c("Greg","Verb","Jeremy","Austin","Zach","Matt","Joe","Adam","Fernando","Brett","Chuck","Ray Jay")
Team <-  c("Joe")
Position <- c("QB","RB","WR","TE")

#Pull Rosters from Google Sheets
rosters <- 
  expand_grid(Team, Position) %>%
  mutate(players = map2(Team, Position, get_players)) %>%
  unnest(cols=c(players)) 

#Calculate the yearly salary for each player
salary_long <- 
  rosters %>%
  mutate(Salary2020 = future_pmap(list(Salary, 1, Length, Class), get_salary),
         Salary2021 = future_pmap(list(Salary, 2, Length, Class), get_salary),
         Salary2022 = future_pmap(list(Salary, 3, Length, Class), get_salary),
         Salary2023 = future_pmap(list(Salary, 4, Length, Class), get_salary),
         Salary2024 = future_pmap(list(Salary, 5, Length, Class), get_salary),
         Guarantee2020 = future_pmap(list(Salary2020, 1, Class), get_guarantee),
         Guarantee2021 = future_pmap(list(Salary2021, 2, Class), get_guarantee),
         Guarantee2022 = future_pmap(list(Salary2022, 3, Class), get_guarantee),
         Guarantee2023 = future_pmap(list(Salary2023, 4, Class), get_guarantee),
         Guarantee2024 = future_pmap(list(Salary2024, 5, Class), get_guarantee)) %>%
  #select(-Salary) %>%
  pivot_longer(cols = Salary2020:Guarantee2024, names_to = "Year", values_to = "Value") %>%
  mutate(merge_name = dp_cleannames(Player, lowercase = TRUE),
         Type = ifelse(grepl("Salary", Year), "Salary", "Guarantee"),
         Year = as.numeric(str_remove(Year, "Salary|Guarantee")),
         Value = as.numeric(Value)) %>% 
  left_join(ktc_values, by = c("merge_name" = "Player")) %>% 
  mutate(value_per_dollar = KTC_Value / Value,
         PotentialValue = KTC_Value/Salary,
         compareValues = if_else(Value == 0, PotentialValue, value_per_dollar))

#Join in KTC values
rosters2021 <- 
  salary_long %>% 
  filter(Year == 2021, Type == "Salary")
  #filter(Year == 2021, Value > 0, Type == "Salary")

salary_long %>%
  filter(Year == 2021, Type == "Salary") %>%
  mutate(PotentialValue = KTC_Value/Salary) %>%     
  ggplot(aes(x=Position, y=PotentialValue)) +
  theme_minimal() +
  ggbeeswarm::geom_quasirandom(aes(size = Salary)) +
  ggrepel::geom_label_repel(data = subset(salary_long %>% mutate(PotentialValue = KTC_Value/Salary),
                                          Year == 2021 & Type == "Salary" & Team == "Joe"),
                            aes(label = Player),
                            nudge_x = 0.4,
                            segment.colour = "red")

rosters2021_transition <- 
  salary_long %>% 
  #filter(Year == 2021, Type == "Salary") %>%
  #filter(Year == 2021, Type == "Salary", Team %in% c("Joe","Adam"))
  filter(Year == 2021, Type == "Salary", Team %in% c("Joe","Chuck"))

#Salaries by position
team_salaries <- 
  salary_long %>%
  #filter(Type == "Salary") %>%
  group_by(Year,Type, Position) %>%
  summarise(Total = sum(Value)) %>%
  ungroup()

#Plot Guaranteed deals
team_salaries %>%
  filter(Type == "Salary") %>%
  ggplot(aes(Year,Total, fill=Position)) +
  geom_area() + 
  geom_area(data = temp %>% filter(Type == "Guarantee"), alpha = 0.5) +
  scale_fill_brewer(palette="Spectral") +
  theme_gdocs()

#Plot total contract type
salary_long %>%
  group_by(Year, Type) %>%
  summarise(Total = sum(Value)) %>%
  ungroup() %>%
  ggplot(aes(Year,Total, color=Type)) +
  geom_line()
  
rosters %>%
  group_by(Team) %>%
  tally()

rosters %>%
  group_by(Length) %>%
  tally()

rosters %>%
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

rosters %>%
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


rosters %>%
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


  
rosters %>%
  ggplot(aes(x= Salary, y = as.factor(Length))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
#geom_density_ridges()

rosters %>%
  ggplot(aes(x= Salary, y = as.factor(Class), fill = Length)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
#geom_density_ridges()

rosters %>%
  ggplot(aes(x= Salary, y = as.factor(Class), fill = Length)) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c()

  write_csv(rosters, "auctioneer_salaries.csv")
