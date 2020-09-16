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
options(warn=-1)
#options(error=recover)
#options(shiny.trace = TRUE)

get_players_sorted <- function(data, var) {
  
  data %>%
    group_by(Name) %>% 
    summarise(total = sum(.data[[var]], na.rm = TRUE)) %>% 
    arrange(desc(total)) %>%
    select(Name) %>% 
    pull()
}

setwd(here::here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% filter(Season >= 2018)
vars <- epdata %>% select(contains("pass"), contains("rush"), contains("rec"), contains("total")) %>% colnames()
week_seasons <- epdata %>% arrange(Season, Week) %>% distinct(week_season) %>% as_vector()

week_master <- epdata %>%
  select(Season, Week, week_season, week_season_num) %>%
  distinct()

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  navbar = ui_header("Expected Points App"),
  sidebar = ui_sidebar(
    menuItem('Weekly Chart',tabName = 'weekly',icon = 'chart-line'),
    menuItem('Data Tables',tabName = 'data', icon = 'table'),
    menuItem('Trends',tabName = 'trends',icon = 'send'),
    menuItem('League Analysis',tabName = 'league',icon = 'trophy'),
    menuItem('About',tabName = 'about',icon = 'question-circle')
  ),
  dashboardBody(
    tabItems(
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
                           pickerInput("selectTeam",
                                       "Select Team:",
                                       choices = sort(unique(epdata$Team)),
                                       selected = "SEA",
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE)),
                           pickerInput("selectPos",
                                       "Select Position:",
                                       choices = c("QB", "RB", "WR", "TE"),
                                       selected = c("RB","WR","TE"),
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))
                    ),
                    column(width = 4,
                           pickerInput("selectSeason",
                                       "Select Season:",
                                       choices = sort(unique(epdata$Season), TRUE),
                                       selected = c("2019","2020"),
                                       multiple = TRUE),
                           pickerInput("selectPlayers",
                                       "Select Players:",
                                       choices = sort(unique(epdata$Name)),
                                       #selected = c("Tyler Lockett", "DK Metcalf"),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE),
                                       multiple = TRUE)
                    )
                    
                  )
              ),
              box(title = "Plot",
                  width = 12,
                  fluidRow(width = 12,
                           plotOutput("pivotGraph", height = "600px"))),
              box(title = "Table",
                  width = 12,
                  fluidRow(width = 12,
                           reactableOutput("teamPivot", width = "100%")))
      ),
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
                                        status = "danger")),
                    column(width = 4,
                           pickerInput("selectTeam2",
                                       "Select Team:",
                                       choices = sort(unique(epdata$Team)),
                                       selected = "SEA",
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE)),
                           pickerInput("selectPos2",
                                       "Select Position:",
                                       choices = c("QB", "RB", "WR", "TE"),
                                       selected = c("RB","WR","TE"),
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))),
                    column(width = 3,
                           pickerInput("selectSeason2",
                                       "Select Season:",
                                       choices = sort(unique(epdata$Season), TRUE),
                                       selected = "2020",
                                       multiple = TRUE),
                           pickerInput("selectPlayers2",
                                       "Select Players:",
                                       choices = sort(unique(epdata$Name)),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE),
                                       multiple = TRUE)),
                    box(title = "Table",
                        width = 12,
                        fluidRow(width = 12,
                                 reactableOutput("table", width = "100%")))
                  )
              )
              
      )
    )
  )
)
  

server <- function(input, output, session) {
  
  weeklyEP <- reactive({
    epdata %>%
      filter(Team %in% input$selectTeam,
             Pos %in% input$selectPos,
             Season %in% input$selectSeason) %>%
      select(Season, Week, week_season, week_season_num, gsis_id, Name, Team, Pos, input$selectVar)

  })
  
  observeEvent({input$selectTeam
    input$selectPos
    input$selectSeason
    input$selectVar},{
      updatePickerInput(session, 'selectPlayers',
                           choices = get_players_sorted(weeklyEP(), input$selectVar),
                           selected = head(get_players_sorted(weeklyEP(), input$selectVar),5))
    })
  
  weeklyEP_playerfilter <- reactive({
    req(input$selectPlayers)
    
    weeklyEP() %>% 
      filter(Name %in% input$selectPlayers)
  })
  
  weekPivot <- reactive({
    weeklyEP_playerfilter() %>%
      arrange(week_season_num) %>%
      select(-week_season_num, -Week, -Season) %>%
      group_by(gsis_id) %>%
      mutate(Total = sum(.data[[input$selectVar]], na.rm = TRUE),
             Average = mean(.data[[input$selectVar]], na.rm = TRUE)) %>%
      pivot_wider(names_from = week_season,
                  values_from = input$selectVar) %>%
      ungroup() %>%
      select(Name, Team, Pos, contains("Week"), Total, Average) %>%
      mutate_if(is.numeric, round, digits = 1) %>%
      arrange(desc(Average))
  })
  
  output$pivotGraph <- renderPlot({
    pivotgraph_data <- weeklyEP_playerfilter() %>%
      group_by(gsis_id) %>%
      mutate(player_weeks = n()) %>%
      ungroup() %>%
      group_by(Season, Week, week_season) %>%
      mutate(week_season_num = cur_group_id(),
             week_season_num = case_when(player_weeks > 2 ~ week_season_num,
                                         TRUE ~ 0L)) %>%
      ungroup() %>%
      mutate(week_season = fct_reorder(week_season, week_season_num))
    
    #plot_breaks <- unique(pivotgraph_data$week_season)
    
    pivotgraph_data %>% 
      ggplot(aes(x = week_season, y = .data[[input$selectVar]], color = Name)) +
      geom_point(size = 3) +
      theme_bw() + 
      labs(x="Week", y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
      theme(plot.title = element_text(face='bold'),
            panel.spacing = unit(0,"lines"),
            text = element_text(size=18),
            axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_y_continuous(limits = c(floor(min(pivotgraph_data[[input$selectVar]])),
                                    ceiling(max(pivotgraph_data[[input$selectVar]])))) +

      #ylim(floor(min(.data[[input$selectVar]])), ceiling(max(.data[[input$selectVar]]))) +
      
      # scale_x_discrete(breaks = plot_breaks[seq(1, by = case_when(length(plot_breaks) < 22 ~ 2,
      #                                                             length(plot_breaks) < 44 ~ 4,
      #                                                             length(plot_breaks) < 66 ~ 8,
      #                                                             TRUE ~ 16),
      #                                           to = length(plot_breaks))]) +
      if(input$pivot_trendlines){geom_smooth(aes(x=week_season_num, y = .data[[input$selectVar]]),
                                             method = "loess", na.rm = TRUE, fill = NA)} 
  })
  
  get_season_weeks <- function(year){
    weekPivot() %>% 
      select(contains(year)) %>% 
      colnames()
  }
  
  create_colgroups <- function(years){
    bucket <- c()

    for (i in 1:length(years)) {
      bucket[[i]] <- colGroup(name = years[i], get_season_weeks(years[i]))
    }

    bucket
  }
  
  output$teamPivot <- renderReactable({
    #print(class(create_colgroups(input$selectSeason)[1]))
    
    weekPivot() %>% 
      reactable(
        defaultColDef = colDef(
          #header = function(value) gsub("\\,.*", "", value),
          header = function(value) str_extract(value, "\\d*?(?=,)"),
          #cell = function(value) format(value, nsmall = 1),
          align = "center"
          #maxWidth = 70,
          #headerStyle = list(background = "#f7f7f8")
        ),
        columnGroups = create_colgroups(input$selectSeason),
        columns = list(
          Name = colDef(header = function(value) value,
                        style = sticky_style(),
                        headerStyle = sticky_style()),
          Team = colDef(header = function(value) value),
          Pos = colDef(header = function(value) value),
          Total = colDef(header = function(value) value),
          Average = colDef(header = function(value) value,
                           style = sticky_style(left = FALSE),
                           headerStyle = sticky_style(left = FALSE))
        ),
        bordered = TRUE,
        highlight = TRUE,
        #style = list(maxWidth = "100%"),
        #searchable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        compact = TRUE,
        width = "100%"
        #,width = 1750
      )
    
    
  })
  
  weeklyEP2 <- reactive({
    field_names <- switch(input$selectCol,
                          "Exp Points" = rlang::exprs(ends_with("x")),
                          "Pass Stats" = rlang::exprs(starts_with("pass")),
                          "Rush Stats" = rlang::exprs(starts_with("rush")),
                          "Rec Stats" = rlang::exprs(starts_with("rec")),
                          "Total Stats" = rlang::exprs(starts_with("total")),
                          "AY Stats" = rlang::exprs(starts_with("rec"), starts_with("total"), starts_with("pass")))
    
    arrange_field <- switch(input$selectCol,
                          "Exp Points" = rlang::exprs(total_fp_x),
                          "Pass Stats" =  rlang::exprs(pass_fp_x),
                          "Rush Stats" =  rlang::exprs(rush_fp_x),
                          "Rec Stats" =  rlang::exprs(rec_fp_x),
                          "Total Stats" =  rlang::exprs(total_fp_x),
                          "AY Stats" = rlang::exprs(WOPR))
    
    epdata %>%
      filter((Team %in% input$selectTeam2),
             (Pos %in% input$selectPos2),
             Season %in% input$selectSeason2) %>%
      {if (input$selectCol == "AY Stats")
        select(., Season, Week, Name, Team, Pos, !!!field_names)
        else select(., Season, Week, Name, Team, Pos, sort(!!!field_names),
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
  
  observeEvent({input$selectTeam2
    input$selectPos2
    input$selectSeason2},{
      updatePickerInput(session, 'selectPlayers2',
                        choices = weeklyEP2()$Name,
                        selected = weeklyEP2()$Name)
    })
  
  weeklyEP2_playerfilter <- reactive({
    req(input$selectPlayers2)
    
    weeklyEP2() %>% 
      filter(Name %in% input$selectPlayers2)
  })
  
  sticky_style <- function(left = TRUE) {
    style <- list(position = "sticky", background = "#f7f7f8", zIndex = 1)
    if (left) {
      style <- c(style, list(left = 0, borderRight = "1px solid #eee"))
    } else {
      style <- c(style, list(right = 0, borderLeft = "1px solid #eee"))
    }
    style
  }
  
  output$table <- renderReactable({
    weeklyEP2_playerfilter() %>% 
      reactable(
        defaultColDef = colDef(
          header = function(value) str_to_upper(gsub("_", " ", value, fixed = TRUE)),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          #minWidth = 90,
          #headerStyle = list(background = "#f7f7f8")
        ),
        # columns = list(
        #   Name = colDef(minWidth = 150)  # overrides the default
        # ),
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
