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
})

source('../calculator-internal/fn_ui_desktop.R')

setwd(here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% filter(season >= 2017)
vars <- epdata %>% select(contains("pass"), contains("rush"), contains("rec"), contains("total")) %>% colnames()
week_seasons <- epdata %>% arrange(season, week) %>% distinct(week_season) %>% as_vector()

week_master <- epdata %>%
  select(season, week, week_season, week_season_num) %>%
  distinct()


ui <- dashboardPage(
  sidebar_collapsed = FALSE,
  navbar = ui_header(),
  sidebar = ui_sidebar(
    menuItem('Weekly Breakdowns',tabName = 'weekly',icon = 'chart-line'),
    menuItem('Data Tables',tabName = 'data', icon = 'table'),
    menuItem('Trends',tabName = 'trends',icon = 'send'),
    menuItem('League Analysis',tabName = 'league',icon = 'trophy'),
    menuItem('About',tabName = 'about',icon = 'question-circle')
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'weekly',
              titlePanel('Weekly Breakdowns'),
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
                                          selected = "RB"),
                           selectizeInput("selectPlayers",
                                          "Select Players:",
                                          choices = c("All"),
                                          selected = "All",
                                          multiple = TRUE)
                    ),
                    column(width = 4,
                           selectInput("selectSeason",
                                       "Select Season:",
                                       choices = sort(unique(epdata$season), TRUE),
                                       selected = "2019",
                                       multiple = TRUE),
                           sliderTextInput("selectWeeks",
                                           "Select Week Range:",
                                           choices = week_seasons,
                                           selected = week_seasons[c(1,21)],
                                           width = '150%'
                           )
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
    min_week <- week_master %>% filter(week_season == input$selectWeeks[1]) %>% select(week_season_num)
    max_week <- week_master %>% filter(week_season == input$selectWeeks[2]) %>% select(week_season_num)

    epdata %>%
      filter(team == input$selectTeam,
             gsis_pos %in% input$selectPos,
             season %in% input$selectSeason,
             between(week_season_num, min_week, max_week))
    
  })
  
  weekPivot <- reactive({
    weeklyEP() %>%
      arrange(week_season_num) %>%
      group_by(gsis_id) %>%
      mutate(Total = sum(.[[input$selectVar]], na.rm = TRUE),
             Average = mean(.[[input$selectVar]], na.rm = TRUE)) %>%
      select(gsis_id, gsis_name, team, gsis_pos, week_season, input$selectVar, Total, Average) %>%
      pivot_wider(names_from = week_season,
                  values_from = input$selectVar) %>%
      ungroup() %>%
      select(gsis_name, team, gsis_pos, contains("Week"), Total, Average) %>%
      mutate_if(is.numeric, round, digits = 1) %>%
      arrange(desc(Average))
  })
  
  observeEvent({input$selectSeason},{
    week_seasons_reactive <-
      epdata %>%
      filter(season %in% input$selectSeason) %>%
      arrange(season, week) %>%
      distinct(week_season) %>%
      as_vector()
    
    updateSliderTextInput(
      session = session,
      inputId = "selectWeeks",
      choices = as.character(week_seasons_reactive)
    )
  })
  
  output$pivotGraph <- renderPlot({

    weeklyEP() %>%
      group_by(gsis_id) %>%
      mutate(player_weeks = n()) %>%
      ungroup() %>%
      group_by(season, week, week_season) %>%
      #arrange(season, week) %>%
      mutate(week_season_num = cur_group_id(),
             week_season_num = case_when(player_weeks > 2 ~ week_season_num,
                                         TRUE ~ 0L)) %>%
      ungroup() %>%
      mutate(week_season = fct_reorder(week_season, week_season_num)) %>%
      #arrange(week_season_num) %>%
      #mutate(Week = factor())
      ggplot(aes(x = week_season, y = .data[[input$selectVar]], color = gsis_name)) +
      geom_point(size = 3) +
      theme_bw() + 
      labs(x="Week", y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
      theme(plot.title = element_text(face='bold'),
            #axis.text.x = element_text(angle=45, hjust=1),
            text = element_text(size=18)) +
      {if(max(.data$week_season_num) < 20) scale_x_discrete(guide = guide_axis(angle = 45))
        else if(max(.data$week_season_num) > 20) scale_x_discrete(guide = guide_axis(n.dodge = 2))} + 
      if(input$pivot_trendlines){geom_smooth(aes(x=week_season_num, y=.data[[input$selectVar]]),
                                             method = "loess", na.rm = TRUE, fill = NA)}
  })
  
  output$teamPivot <- renderDT({
    #req(input$weeklyRadio == "Weekly")
    
    weekPivot() %>% 
      datatable(
        rownames=T,
        options(
          scrollX=TRUE,
          paging=FALSE,
          searching=FALSE)) #%>%
      #formatRound(columns = c(2:ncol(selCols())), digits = if (grepl('share',input$selectVar)) {3} else {1})
  })
  
}

shinyApp(ui, server)