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
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)
  library(reactable)
})

source('fn_ui_desktop.R')

setwd(here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% filter(season >= 2018)
vars <- epdata %>% select(contains("pass"), contains("rush"), contains("rec"), contains("total")) %>% colnames()
week_seasons <- epdata %>% arrange(season, week) %>% distinct(week_season) %>% as_vector()

week_master <- epdata %>%
  select(season, week, week_season, week_season_num) %>%
  distinct()


ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  navbar = ui_header(),
  sidebar = ui_sidebar(
    menuItem('Data Tables',tabName = 'data', icon = 'table'),
    menuItem('Weekly Chart',tabName = 'weekly',icon = 'chart-line'),
    menuItem('Trends',tabName = 'trends',icon = 'send'),
    menuItem('League Analysis',tabName = 'league',icon = 'trophy'),
    menuItem('About',tabName = 'about',icon = 'question-circle')
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'data',
              titlePanel("Data Tables"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width = 5,
                           radioGroupButtons("selectCol",
                                             "Select Columns:",
                                             choices = c("Exp Points","Pass Stats","Rush Stats","Rec Stats","Total Stats","AY Stats"),
                                             selected = "Exp Points",
                                             status = "danger"),
                           awesomeRadio("weeklyRadio",
                                        "Data type",
                                        choices = c("Weekly","Weekly Average","Totals"),
                                        selected = "Weekly Average",
                                        status = "danger")
                           
                           
                    ),
                    column(width = 4,
                           selectizeInput("selectTeam2",
                                          "Select Team:",
                                          choices = c("All", sort(unique(epdata$team))),
                                          selected = "SEA"),
                           selectizeInput("selectPos2",
                                          "Select Position:",
                                          choices = c("All", "QB", "RB", "WR", "TE"),
                                          selected = "WR")
                    ),
                    column(width = 3,
                           pickerInput("selectSeason2",
                                       "Select Season:",
                                       choices = sort(unique(epdata$season), TRUE),
                                       selected = "2020",
                                       multiple = TRUE),
                           selectizeInput("selectPlayers2",
                                          "Select Players:",
                                          choices = c("All"),
                                          selected = "All",
                                          multiple = TRUE)
                    )
                    
                  )                    
              ),
              box(title = "Table",
                  width = 12,
                  fluidRow(width = 12,
                           reactableOutput("table"))
              )
      ),
      tabItem(tabName = 'weekly',
              titlePanel('Weekly Charts'),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           selectizeInput("selectVar",
                                          "Select Variable:",
                                          choices = vars,
                                          selected = "total_fp_x"),
                           checkboxInput("pivot_trendlines", label = "Display Trendlines", value = TRUE)
                    ),
                    column(width = 4,
                           selectizeInput("selectTeam",
                                          "Select Team:",
                                          choices = c("All", sort(unique(epdata$team))),
                                          selected = "KC"),
                           selectizeInput("selectPos",
                                          "Select Position:",
                                          choices = c("All", "QB", "RB", "WR", "TE"),
                                          selected = "RB")
                    ),
                    column(width = 4,
                           pickerInput("selectSeason",
                                       "Select Season:",
                                       choices = sort(unique(epdata$season), TRUE),
                                       selected = "2020",
                                       multiple = TRUE),
                           selectizeInput("selectPlayers",
                                          "Select Players:",
                                          choices = c("All"),
                                          selected = "All",
                                          multiple = TRUE)
                           # ,sliderTextInput("selectWeeks",
                           #                 "Select Week Range:",
                           #                 choices = week_seasons,
                           #                 selected = week_seasons[c(1,21)],
                           #                 width = '150%'
                           # )
                    )
                    
                  )
              ),
              box(title = "Plot",
                  width = 12,
                  fluidRow(width = 12,
                           plotOutput("pivotGraph", height = "600px"))
              ),
              box(title = "Table",
                  width = 12,
                  fluidRow(width = 12,
                           DTOutput("teamPivot"))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  weeklyEP <- reactive({
    # min_week <- week_master %>% filter(week_season == input$selectWeeks[1]) %>% select(week_season_num)
    # max_week <- week_master %>% filter(week_season == input$selectWeeks[2]) %>% select(week_season_num)

    epdata %>%
      filter((team == input$selectTeam | input$selectTeam == "All"),
             (gsis_pos %in% input$selectPos | input$selectPos == "All"),
             season %in% input$selectSeason
             #,between(week_season_num, min_week, max_week)
             ) %>%
      select(season, week, week_season, week_season_num, gsis_id, gsis_name, team, gsis_pos, input$selectVar)
    
  })
  
  weekPivot <- reactive({
    weeklyEP() %>%
      arrange(week_season_num) %>%
      select(-week_season_num, -week, -season) %>%
      group_by(gsis_id) %>%
      mutate(Total = sum(.data[[input$selectVar]], na.rm = TRUE),
             Average = mean(.data[[input$selectVar]], na.rm = TRUE)) %>%
      pivot_wider(names_from = week_season,
                  values_from = input$selectVar) %>%
      ungroup() %>%
      select(gsis_name, team, gsis_pos, contains("Week"), Total, Average) %>%
      mutate_if(is.numeric, round, digits = 1) %>%
      arrange(desc(Average))
  })
  
  # observeEvent({input$selectSeason},{
  #   week_seasons_reactive <-
  #     epdata %>%
  #     filter(season %in% input$selectSeason) %>%
  #     arrange(season, week) %>%
  #     distinct(week_season) %>%
  #     as_vector()
  #   
  #   updateSliderTextInput(
  #     session = session,
  #     inputId = "selectWeeks",
  #     choices = as.character(week_seasons_reactive)
  #   )
  # })
  
  output$pivotGraph <- renderPlot({
    pivotgraph_data <<- weeklyEP() %>%
      group_by(gsis_id) %>%
      mutate(player_weeks = n()) %>%
      ungroup() %>%
      group_by(season, week, week_season) %>%
      mutate(week_season_num = cur_group_id(),
             week_season_num = case_when(player_weeks > 2 ~ week_season_num,
                                         TRUE ~ 0L)) %>%
      ungroup() %>%
      mutate(week_season = fct_reorder(week_season, week_season_num))
    
    plot_breaks <- unique(pivotgraph_data$week_season)
    
    pivotgraph_data %>% 
      ggplot(aes(x = week_season, y = .data[[input$selectVar]], color = gsis_name)) +
      geom_point(size = 3) +
      theme_bw() + 
      labs(x="Week", y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
      theme(plot.title = element_text(face='bold'),
            panel.spacing = unit(0,"lines"),
            text = element_text(size=18)) +
      scale_x_discrete(breaks = plot_breaks[seq(1, by = case_when(length(plot_breaks) < 22 ~ 2,
                                                                  length(plot_breaks) < 44 ~ 4,
                                                                  length(plot_breaks) < 66 ~ 8,
                                                                  TRUE ~ 16),
                                                to = length(plot_breaks))]) +
      if(input$pivot_trendlines){geom_smooth(aes(x=week_season_num, y = pivotgraph_data[[input$selectVar]]),
                                             method = "loess", na.rm = TRUE, fill = NA)} 
  })
  
  output$teamPivot <- renderDT({
    #req(input$weeklyRadio == "Weekly")
    
    weekPivot() %>% 
      datatable(
        class = "compact stripe nowrap",
        rownames=T,
        options(
          scrollX=TRUE,
          paging=FALSE,
          searching=FALSE))
  })
  
  weeklyEP2 <- reactive({
    field_names <- switch(input$selectCol,
                          "Exp Points" = rlang::exprs(ends_with("x")),
                          "Pass Stats" = rlang::exprs(starts_with("pass")),
                          "Rush Stats" = rlang::exprs(starts_with("rush")),
                          "Rec Stats" = rlang::exprs(starts_with("rec")),
                          "Total Stats" = rlang::exprs(starts_with("total")),
                          "AY Stats" = rlang::exprs(starts_with("rec"), starts_with("total"), starts_with("pass")))
    
                          #"AY Stats" = rlang::exprs(rec_ay,rec_tar,rec_yd,total_fp_x,total_fp,pass_ay_team,pass_att_team,total_fp_team_x,total_fp_team))
    
    arrange_field <- switch(input$selectCol,
                          "Exp Points" = rlang::exprs(total_fp_x),
                          "Pass Stats" =  rlang::exprs(pass_fp_x),
                          "Rush Stats" =  rlang::exprs(rush_fp_x),
                          "Rec Stats" =  rlang::exprs(rec_fp_x),
                          "Total Stats" =  rlang::exprs(total_fp_x),
                          "AY Stats" = rlang::exprs(WOPR))
    
    epdata %>%
      filter((team == input$selectTeam2 | input$selectTeam2 == "All"),
             (gsis_pos %in% input$selectPos2 | input$selectPos2 == "All"),
             season %in% input$selectSeason2) %>%
      {if (input$selectCol == "AY Stats")
        select(., Season = season, Week = week, Name = gsis_name, Team = team, Pos = gsis_pos, sort(!!!field_names))
        else select(., Season = season, Week = week, Name = gsis_name, Team = team, Pos = gsis_pos, sort(!!!field_names),
                    -contains("_team"), -contains("proxy"))} %>%
      {if (input$selectCol == "AY Stats")
        group_by(., Name, Team, Pos) %>% 
          summarise(games = n(),
                    rec_comp = sum(rec_comp, na.rm = TRUE),
                    rec_tar =  sum(rec_tar, na.rm = TRUE),
                    rec_yd = sum(rec_yd, na.rm = TRUE),
                    rec_ay = sum(rec_ay, na.rm = TRUE),
                    rec_td = sum(rec_td, na.rm = TRUE),
                    rec_adot = sum(rec_ay, na.rm = TRUE) / sum(rec_tar, na.rm = TRUE),
                    rec_ay_share = sum(rec_ay, na.rm = TRUE) / sum(pass_ay_team, na.rm = TRUE),
                    rec_tgt_share = sum(rec_tar, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE),
                    WOPR = 1.5*sum(rec_tar, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE) + 0.7*sum(rec_ay, na.rm = TRUE) / sum(pass_ay_team, na.rm = TRUE),
                    RACR = sum(rec_yd, na.rm = TRUE) / sum(rec_ay, na.rm = TRUE),
                    YPTPA = sum(rec_yd, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE),
                    total_fp_x_share = sum(total_fp_x, na.rm = TRUE) / sum(total_fp_team_x, na.rm = TRUE),
                    total_fp_share = sum(total_fp, na.rm = TRUE) / sum(total_fp_team, na.rm = TRUE))
        else if (input$weeklyRadio == "Weekly Average")
          group_by(., Name, Team, Pos) %>%
          summarise(games = n(),
                    across(!!!field_names, ~mean(.x, na.rm = TRUE))) 
        else if (input$weeklyRadio == "Totals")
          group_by(., Name, Team, Pos) %>%
          summarise(games = n(),
                    across(!!!field_names, ~sum(.x, na.rm = TRUE)))
        else .} %>% 
      ungroup() %>% 
      {if (input$selectCol == "AY Stats")
        mutate(., across(where(is.numeric), ~round(.x, 2)))
        else mutate(., across(where(is.numeric), ~round(.x, 1))) } %>% 
      arrange(-(!!!arrange_field))
    
  })
  
  # output$table <- renderDT({
  #   #req(input$weeklyRadio == "Weekly")
  #   
  #   weeklyEP2() %>% 
  #     datatable(
  #       class = "compact stripe nowrap",
  #       rownames=T,
  #       options(
  #         scrollX=TRUE,
  #         paging=FALSE,
  #         searching=FALSE))
  # })
  
  
  output$table <- renderReactable({
    weeklyEP2() %>% 
      reactable(
        defaultColDef = colDef(
          header = function(value) str_to_upper(gsub("_", " ", value, fixed = TRUE)),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          minWidth = 90,
          #headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          Name = colDef(minWidth = 150)  # overrides the default
        ),
        bordered = TRUE,
        highlight = TRUE,
        #searchable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        compact = TRUE,
        width = "100%"
      )
    
    
  })
}

shinyApp(ui, server)