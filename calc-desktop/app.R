suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(RSQLite)
  library(pool)
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(uuid)
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyMobile)
  library(shinyWidgets)
  library(DT)
  library(sever) # johncoene/sever
  library(joker) # tanho63/joker
})

# Read data from local (see update_local.R) ----

players_raw <- read_parquet('../calculator-internal/player_raw.pdata')
picks_raw <- read_parquet('../calculator-internal/picks_raw.pdata')

source('../calculator-internal/fn_server.R')
source('../calculator-internal/fn_ui_desktop.R')

pool_localdb <- dbPool(SQLite(),
                       dbname = '../calculator-internal/calculator_log.sqlite',
                       minSize = 10
)

ui <- dashboardPage(
  sidebar_collapsed = FALSE,
  navbar = ui_header(),
  sidebar = ui_sidebar(
    menuItem('Calculator',tabName = 'calculator',icon = 'quidditch'),
    menuItem('Help',tabName = 'help',icon = 'question'),
    menuItem('Popular Trades',tabName = 'popular', icon = 'tachometer-alt'),
    menuItem('About',tabName = 'about',icon = 'rocket')
  ),
  dashboardBody(
    use_sever(),
    tabItems(
      tabItem(tabName = 'calculator',
              fluidRow(
                column(width = 8,
                       uiOutput('team_inputs')),
                column(width = 4,
                       box(width = 12, title = "Calculator Options",inputId = 'calc_options',status = 'danger',collapsible = TRUE,
                        div(style = 'text-align:center;',
                            radioGroupButtons(
                              'qb_type',justified = TRUE,width = '100%',
                              choices = c('1QB','2QB/SF'),
                              selected = '1QB',
                              checkIcon = list(yes = icon('check')))),
                        pickerInput(
                          'teams', width = '100%',
                          choices = glue("{seq(6,24,2)} teams"),
                          selected = '12 teams'),
                        pickerInput(
                          'draft_type', width = '100%',
                          choices = c('Normal', 'Startup (Players & Picks)','Startup (Players Only)')),
                        pickerInput(
                          'calc_type', width = '100%',
                          choices = c("I'm considering this trade", "I've received this offer", "I've completed this trade"))
                        ),
                       box(width = 12, title = "Value Controls", status = 'danger', collapsible = TRUE,
                        h6('Value Factor'),
                        iconwrap_slider(
                          'value_factor',label = NULL, min = 210, max = 260, value = 235,
                          ticks = FALSE,step = 5,width = '100%', icon_left = 'layer-group', icon_right = 'star'),
                        h6('Rookie Pick Optimism'),
                        iconwrap_slider(
                          'rookie_optimism',label = NULL, min = 0, max = 100, value = 80,
                          ticks = FALSE, step = 5, width = '100%', icon_left = 'snowflake', icon_right = 'hotjar'),
                        h6('Future Factor'),
                        iconwrap_slider(
                          'future_factor',label = NULL, min = 65, max = 95, value = 80,
                          ticks = FALSE, step = 5, width = '100%', icon_left = 'play', icon_right = 'fast-forward')
                        )
                    ),
                column(width=12,
                       fluidRow(
                         box(
                           title = 'Analysis', maximizable = TRUE, collapsible = FALSE, width = 4
                           ),
                         box(
                           title = 'Plot', maximizable = TRUE, collapsible = FALSE, width = 8
                           ),
                         box(
                           title = 'Wizard', maximizable = TRUE, collapsible = FALSE,width = 4
                           )
                         ))
                )),
      tabItem(tabName = 'help'),
      tabItem(tabName = 'popular'),
      tabItem(tabName = 'about')
  
  
)))

server <- function(input, output, session) {

  sever_joke()

  # Render Inputs ----
  output$teamAinput <- renderUI({
    pickerInput(
      'players_teamA',
      choices = values()$Player,
      options = list(
        title = "Select Players for Team A!",
        `selected-text-format` = "count > 0",
        `actions-box` = TRUE,
        `live-search` = TRUE),
      multiple = TRUE,
      width = '100%'
    )
    })
  output$teamBinput <- renderUI({
    pickerInput(
      'players_teamB',
      choices = values()$Player,
      options = list(
        title = "Select Players for Team B!",
        `selected-text-format` = "count > 0",
        `actions-box` = TRUE,
        `live-search` = TRUE),
      multiple = TRUE,
      width = '100%'
    )
  })
  
  output$teamA_list <- renderUI({
    req(input$players_teamA)
    
    map(input$players_teamA,bs4ListGroupItem,type = 'basic') %>% 
      bs4ListGroup(width = 12)
  })
  output$teamB_list <- renderUI({
    req(input$players_teamB)
    
    map(input$players_teamB,bs4ListGroupItem,type = 'basic') %>% 
      bs4ListGroup(width = 12)
  })
  
  output$team_inputs <- renderUI({
    
    box(
      width = 12,
      title = 'Players',
      status = 'danger',
      footer = actionBttn(
        'calculate',
        style = 'material-flat',
        block = TRUE,
        color = 'default',
        label = "Calculate!"
      ),
      bs4CardLayout(
        type = 'group',
        box(
          title = 'Team A',
          elevation = 0,
          collapsible = FALSE,
          status = 'green',
          width = NULL,
          uiOutput('teamAinput'),
          uiOutput('teamA_list')
        ),
        box(
          title = 'Team B',
          elevation = 0,
          status = 'purple',
          collapsible = FALSE,
          width = NULL,
          uiOutput('teamBinput'),
          uiOutput('teamB_list')
        )
        
      )
    )
    
    
  })
  
  # Close calc_options box on calculate
  observeEvent(input$calculate,{
    if(!input$calc_options$collapsed){updatebs4Card('calc_options',session,'toggle')}
    })
  
  # hold the selected players if values change (i.e. settings are toggled)
  observeEvent(values(),{
    
    holdA <- input$players_teamA
    holdB <- input$players_teamB
    
    updatePickerInput(session,'players_teamA',selected = holdA)
    updatePickerInput(session,'players_teamB',selected = holdB)
  })
  
  # Calculate Actual Values ----
  values <- reactive({
    gen_df_values(players_raw,picks_raw,
                  input$qb_type,input$teams,input$value_factor,
                  input$rookie_optimism,input$draft_type,input$future_factor, 
                  c('Player','Age','Value'))
  })
  
  # Results ----
  teamA_values <- eventReactive(input$calculate,{
    values() %>%
      filter(Player %in% input$players_teamA) %>%
      arrange(desc(Value))
  })
  teamB_values <- eventReactive(input$calculate,{
    values() %>%
      filter(Player %in% input$players_teamB) %>%
      arrange(desc(Value))
  })
  
  teamA_total <- reactive({
    teamA_values() %>% 
      summarise(Total = sum(Value)) %>% 
      pull(Total)
  })
  teamB_total <- reactive({
    teamB_values() %>% 
      summarise(Total = sum(Value)) %>% 
      pull(Total)
  })
  
  percent_diff <- reactive({if (teamA_total() > teamB_total())
  {round(100*((teamA_total() - teamB_total())/teamB_total()))}
    else if (teamA_total() < teamB_total())
    {round(100*((teamB_total() - teamA_total())/teamA_total()))}
    else
    {0}
  })
  
  
  
  # Save data to a sqlite file on server ----
  
  sessionID <- UUIDgenerate(1)
  
  observeEvent(input$calculate, {

    req(teamA_values(),teamB_values())
    
    saved_data <- {tibble(
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
    )}
    
    try(dbWriteTable(pool_localdb, name = 'calculator_log',value = saved_data,append=TRUE))

  })
  
}

shinyApp(ui, server)
