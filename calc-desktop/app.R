suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(RSQLite)
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(uuid)
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)
  library(mobileCharts) # rinterface/mobileCharts
  library(sever) # johncoene/sever
  library(joker) # tanho63/joker
})

# Read data from local (see update_local.R) ----
# players_raw <- read_parquet('player_raw.pdata')
# picks_raw <- read_parquet('picks_raw.pdata')

ui <- dashboardPage(
  sidebar_collapsed = FALSE,
  dashboardHeader(skin = 'dark',
                  fixed = TRUE,
                  status = 'danger',
                  border = TRUE,
                  span('Trade Calculator',style= 'font-size:1.5em;color:#ffffff')
                  ),
  dashboardSidebar(title = 'DynastyProcess.com',
                   fixed = TRUE,
                   brandColor = 'danger',
                   status = 'danger',
                   elevation = 3,
                   opacity = 0.8,
                   url = "https://dynastyprocess.com",
                   expand_on_hover = TRUE,
                   src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
                   skin = 'light',
                   sidebarMenu(
                     menuItem('Calculator',tabName = 'calculator',icon = 'quidditch'),
                     menuItem('Help',tabName = 'Help',icon = 'question'),
                     menuItem('Popular Trades', icon = 'tachometer-alt'),
                     menuItem('About',icon = 'rocket')
                   )),
  dashboardBody(use_sever(),
                tabItems(
                  tabItem(
                    tabName = 'calculator',
                    column(width = 12,
                           bs4CardLayout(
                             type = 'group',
                             box(
                               title = 'Team A',
                               collapsible = FALSE,
                               # maximizable = TRUE,
                               width = NULL,
                               pickerInput('teamAplayers',choices = c('Player1','Player2','Player3'),multiple = TRUE,width = '100%')
                             ),
                             box(
                               title = 'Team B',
                               # maximizable = TRUE,
                               collapsible = FALSE,
                               width = NULL,
                               pickerInput('teamAplayers',choices = c('Player1','Player2','Player3'),multiple = TRUE,width = '100%')
                             ),
                             box(width = NULL,collapsible = FALSE,br(),br(),div(actionButton('calculate',label = "Calculate!",class = 'btn-success'),style = 'text-align:center;'))
                           )
                    ),
                      box(
                        title = 'Controls',
                        collapsible = FALSE,
                        # maximizable = TRUE,
                        width = 12
                      ),
                    column(width=12,
                      fluidRow(
                        box(
                          title = 'Plot',
                          maximizable = TRUE,
                          collapsible = FALSE,
                          width = 8
                        ),
                        box(
                          title = 'Analysis',
                          # status = 'danger',
                          # solidHeader = TRUE,
                          maximizable = TRUE,
                          collapsible = FALSE,
                          width = 4
                        ),
                        box(
                          title = 'Wizard',
                          # status = 'danger',
                          # solidHeader = TRUE,
                          maximizable = TRUE,
                          collapsible = FALSE,
                          width = 4
                        )
                      )
                      )
                    )))
  
  
)

server <- function(input, output, session) {
  
  # Sever - cleans up the disconnect screen ----
  sever(
    tagList(
      h1("Disconnected"),
      p(em(randomjoke())),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
  
  # Helper functions ----
  
  select_qbtype <- function(df,qb_type){
    df %>% 
      mutate(ecr = case_when(qb_type == '1QB' ~ecr_1qb,
                             TRUE ~ ecr_2qb))
  }
  
  calculate_value <- function(df,value_factor){
    v_f <- value_factor/10000
    
    df %>% 
      mutate(
        value = 10500 * exp(-v_f * ecr),
        value = round(value))
  } 
  
  label_currentpicks <- function(df,leaguesize) {
    l_s <- leaguesize + 0.001
    
    df %>% 
      mutate(
        season = year(as_date(scrape_date)),
        rookie_round = (pick %/% l_s)+1,
        round_pick = round(pick %% l_s,0),
        pick_label = paste0(season," Pick ",as.character(rookie_round),".",str_sub(paste0(0,round_pick),-2,-1))
      )
  }
  
  add_futurepicks <- function(df,futurerookie_factor,leaguesize){
    fr_f <- futurerookie_factor/100
    l_s <- leaguesize + 0.001
    
    n1 <- df %>%
      mutate(season = season + 1,
             rookie_round = case_when(rookie_round == 1 ~ '1st',
                                      rookie_round == 2 ~ '2nd',
                                      rookie_round == 3 ~ '3rd',
                                      rookie_round >= 4 ~ paste0(rookie_round,'th')),
             eml = case_when(round_pick <= l_s/3 ~ 'Early',
                             round_pick <= l_s*2/3 ~ 'Mid',
                             TRUE ~ 'Late'))
    
    n1_eml <- n1 %>% 
      group_by(season,rookie_round,eml) %>% 
      summarise(value = mean(value)*fr_f) %>% 
      ungroup() %>% 
      mutate(pick_label = paste(season,eml,rookie_round))
    
    n1_summary <- n1 %>% 
      group_by(season,rookie_round) %>% 
      summarise(value = mean(value)*fr_f) %>% 
      ungroup() %>% 
      mutate(pick_label = paste(season,rookie_round))
    
    n2_summary <- n1_summary %>% 
      mutate(season = season + 1,
             value = value*fr_f,
             pick_label = paste(season,rookie_round))
    
    df %>% 
      mutate(rookie_round = as.character(rookie_round)) %>% 
      bind_rows(n1_eml,n1_summary,n2_summary) %>% 
      mutate(position = "PICK",
             value = round(value)) %>% 
      arrange(desc(value))
  }
  
  calc_currentrookies <- function(df,rookie_opt,qb_type){
    # r_o <- rookie_opt/100
    df %>% 
      mutate(
        high_model = case_when(qb_type == '1QB' ~ ecr_high_1qb,
                               TRUE ~ ecr_high_2qb), 
        low_model = case_when(qb_type == '1QB' ~ ecr_low_1qb,
                              TRUE ~ ecr_low_2qb), 
        high_factor = rookie_opt/100,
        low_factor = 1-high_factor,
        ecr = high_factor*high_model + low_factor*low_model)
  }
  
  label_displaymode <- function(df,displaymode,leaguesize){
    l_s <- parse_number(leaguesize) + 0.001
    
    if(displaymode=='Normal'){return(df)}
    
    df %>%
      filter(case_when(displaymode == 'Startup (Players Only)'~ pos!='PICK',
                       displaymode == 'Startup (Players & Picks)'~ (pos!='PICK'|grepl(year(Sys.Date()),player)))) %>% 
      arrange(desc(value)) %>% 
      rowid_to_column(var='pick') %>% 
      mutate(startup_round = (pick %/% l_s)+1,
             startup_pick = round(pick %% l_s,0),
             startup_label = paste0("Startup Pick ",startup_round,".",str_sub(paste0(0,startup_pick),-2,-1))) %>% 
      bind_rows(df) %>% 
      mutate(player = coalesce(startup_label,player)) %>% 
      arrange(desc(value),player)
  }
  
  # Calculate Actual Values ----
  
  pickvalues <- reactive({
    picks_raw %>% 
      calc_currentrookies(input$rookie_optimism,input$qb_type) %>% 
      label_currentpicks(parse_number(input$teams)) %>% 
      calculate_value(input$value_factor) %>% 
      add_futurepicks(input$future_factor,parse_number(input$teams)) %>% 
      select(player = pick_label,value)
  })
  
  values <- reactive({
    players_raw %>%  
      select_qbtype(input$qb_type) %>% 
      calculate_value(input$value_factor) %>% 
      bind_rows(pickvalues()) %>% 
      label_displaymode(input$draft_type,input$teams) %>% 
      select(Player = player,Age = age,Value = value) %>%
      arrange(desc(Value))
  })
  
  # Save data to a sqlite file on server ----
  
  sessionID <- UUIDgenerate(1)
  
  observeEvent(input$calculate, {
    
    saved_data <- tibble(
      trade_id = UUIDgenerate(1),
      session_id = sessionID,
      timestamp = Sys.time(),
      input_calctype = input$calc_type,
      input_drafttype = input$draft_type,
      input_qb = input$qb_type,
      input_teams = input$teams,
      input_valuefactor = input$value_factor,
      input_rookieoptimism = input$rookie_optimism,
      input_futurefactor = input$future_factor,
      teamA_players = paste(input$players_teamA, sep = "", collapse = " | "),
      teamA_values = paste0(teamA_values()$Value, sep = "", collapse = " | "),
      teamA_total = teamA_total(),
      teamB_players = paste(input$players_teamB, sep = "", collapse = " | "),
      teamB_values = paste0(teamB_values()$Value, sep = "", collapse = " | "),
      teamB_total = teamB_total()
    )
    
    db_local <- dbConnect(RSQLite::SQLite(),'calculator_log2.sqlite')
    dbWriteTable(db_local,name = 'calculator_log',value = saved_data,append=TRUE)
    dbDisconnect(db_local)
    
  })
  
  
}

shinyApp(ui, server)
