suppressPackageStartupMessages({
  # Data import/export
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
  library(shinyMobile) # tanho63/shinymobile
  # library(echarts4r)
  library(DT)
  library(mobileCharts) # rinterface/mobileCharts
  library(sever) # johncoene/sever
  library(joker) # tanho63/joker
  
})

source('../calculator-internal/fn_ui_mobile.R')
source('../calculator-internal/fn_server.R')

# Read data from local (see update_local.R) ----
players_raw <- read_parquet('../calculator-internal/player_raw.pdata')
picks_raw <- read_parquet('../calculator-internal/picks_raw.pdata')

pool_localdb <- dbPool(SQLite(),
               dbname = '../calculator-internal/calculator_log.sqlite',
               minSize = 10
               )

ui <- f7Page( # f7Page setup and Init Options ----
  title = "DynastyProcess Trade Calculator",
  dark_mode = TRUE,
  manifest = "manifest.json",
  favicon = "favicon.ico",
  icon = '128x128.png',
  init = f7Init(
    skin = 'md',
    theme = 'dark',
    color = 'pink',
    tapHold = FALSE
  ),
  f7TabLayout( # f7TabLayout ----
    use_sever(),
    addcss_transparentDT(),
    navbar = ui_header(),
    panels = ui_sidebar(),
    appbar = NULL,
    f7Tabs( 
      # Main tabs ----
      id = 'tabs',
      f7Tab( # input tab ----
             tabName = "Inputs",
             icon = f7Icon('wand_stars', old = FALSE),
             active = TRUE,
             br(),
             div(img(src = 'icons/128x128.png'), style = 'text-align:center;'),
             
             h1("Main Inputs", style = "text-align:center"),
             uiOutput('team_inputs'),
             
             f7SmartSelect(
               'calc_type',
               label = "Trade Details",
               smart = FALSE,
               choices = c("I'm considering this trade",
                           "I've received this trade offer",
                           "I've completed this trade"
                           )),
             f7Card(title = 'Customize Value Settings',
                    f7Row(
                      f7SmartSelect(
                        'qb_type',
                        label = 'QB Type',
                        choices = c('1QB', '2QB/SF')
                      ),
                      f7SmartSelect(
                        'teams',
                        label = 'Teams',
                        choices = glue("{seq(6,24,2)} teams"),
                        selected = "12 teams"
                      ),
                      f7SmartSelect(
                        'draft_type',
                        label = "Display Mode",
                        choices = c('Normal',
                                    'Startup (Players & Picks)',
                                    'Startup (Players Only)')
                      )
                      
                    ),
                    f7Slider(
                      'value_factor',
                      "Valuation Factor",
                      min = 210,
                      max = 260,
                      value = 235,
                      step = 5,
                      labels = tagList(
                        f7Icon("square_stack_3d_up_fill", old = FALSE),
                        f7Icon("star_circle_fill", old = FALSE)
                      )
                    ),
                    f7Slider(
                      'rookie_optimism',
                      'Rookie Optimism',
                      min = 0,
                      max = 100,
                      value = 80,
                      step = 5,
                      labels = tagList(
                        f7Icon("bolt_slash_fill", old = FALSE),
                        f7Icon("bolt_fill", old = FALSE)
                      )
                    ),
                    f7Slider(
                      'future_factor',
                      'Future Pick Value',
                      min = 65,
                      max = 95,
                      value = 80,
                      step = 5,
                      labels = tagList(
                        f7Icon("play_fill", old = FALSE),
                        f7Icon("forward_fill", old = FALSE)
                      )
                    ),
                    br(),
                    f7Button(
                      'toggle_inputhelp',
                      label = "Help",
                      shadow = TRUE,
                      size = 'small',
                      rounded = TRUE
                    )
             ),
             ui_spacer()
      ),
      f7Tab( # analysis tab ----
        tabName = "Analysis",
        icon = f7Icon('graph_circle_fill', old = FALSE),
        h1("Trade Analysis", style = 'text-align:center;'),
        uiOutput('results_tab')
      ),
      f7Tab(tabName = "Values", # values tab ----
        icon = f7Icon('square_favorites_fill', old = FALSE),
        h1('Values - Quick Reference', style = 'text-align:center;'),
        uiOutput('values')
      ),
      f7Tab(tabName = "About", # about tab ----
        icon = f7Icon('info_circle_fill', old = FALSE),
        br(),
        div(img(src = 'icons/128x128.png'), style = 'text-align:center;'),
        br(),
        f7Card(title = "About",
               includeMarkdown('about.md')),
        br(),
        f7Card(glue(
          "ECR last updated: {players_raw$scrape_date[[1]]}"
        )),
        br(),
        f7Card(
          title = "More by DynastyProcess:",
          f7List(
            inset = TRUE,
            # f7ListItem(title = "Desktop Version",
            #            url = "https://apps.dynastyprocess.com/calculator",
            #            media = f7Icon('number_circle_fill',old = FALSE)),
            f7ListItem(
              title = "Crystal Ball",
              media = f7Icon('moon_circle_fill', old = FALSE),
              url = "https://apps.dynastyprocess.com/crystalball"
            ),
            f7ListItem(
              title = "Twitter",
              media = f7Icon('logo_twitter', old = FALSE),
              url = "https://www.twitter.com/dynastyprocess"
            ),
            f7ListItem(
              title = "Data Repository",
              media = f7Icon('archivebox_fill', old = FALSE),
              url = "https://www.github.com/dynastyprocess/data"
            ),
            f7ListItem(
              title = "Main Site",
              media = f7Icon('waveform_circle_fill', old = FALSE),
              url =
                "https://dynastyprocess.com"
            )
          )
        ),
        
        br()
        # f7Card(title = "Popular Players")
                                   )
                           )
              )) # end of UI tab ----
# start of server ----
server <- function(input, output, session) { 

  sever_joke() # cleans up disconnect screen

  # Calculate Actual Values ----

  values <- reactive({
    gen_df_values(players_raw,picks_raw,
                  input$qb_type,input$teams,input$value_factor,
                  input$rookie_optimism,input$draft_type,input$future_factor, 
                  c('Player','Age','Value'))
  })
  
  # Render input fields ----
  
  output$teamAinput <- renderUI({
    f7AutoComplete('players_teamA',
                   label = "Add Players to Team A",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = values()$Player,
                   # value = NULL
                   value = values()$Player[sample(1:32,1)]
                   )})
  
  output$teamBinput <- renderUI({
    f7AutoComplete('players_teamB',
                   label = "Add Players to Team B",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = values()$Player,
                   # value = NULL
                   value = values()$Player[sample(1:32,1)]
                   )})
  
  output$teamA_list <- renderUI({
    req(input$players_teamA)
    
    map(input$players_teamA,f7ListItem) %>% 
      f7List(inset = TRUE)
  })
  
  output$teamB_list <- renderUI({
    req(input$players_teamB)
    
    map(input$players_teamB,f7ListItem) %>% 
      f7List(inset = TRUE)
  })
  
  output$team_inputs <- renderUI({
    
    div(
      uiOutput('teamAinput'),
      uiOutput('teamA_list'),
      uiOutput('teamBinput'),
      uiOutput('teamB_list'),
      f7Button('calculate',"Calculate!",
               shadow = TRUE)
    )
  })
  
  observeEvent(input$toggle_inputhelp, {
    updateF7Panel(inputId = "panel_left", session = session)
  })
  
  observeEvent(values(),{
    
    holdA <- input$players_teamA
    holdB <- input$players_teamB
    
    updateF7AutoComplete('players_teamA',value = holdA)
    updateF7AutoComplete('players_teamB',value = holdB)
  })
  
  # Results tab ----
  
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
  
  output$trade_gauge <- renderUI({
    
    gauge_value <- if(teamA_total() > teamB_total()){50+percent_diff()/2} else {50-percent_diff()/2}

    f7Gauge('score',type = 'semicircle', value = gauge_value,
            borderBgColor = '#1b7837',
            borderColor = '#762a83',
            valueText = paste0(percent_diff(),'%'),
            valueTextColor = case_when(teamA_total() == teamB_total() ~ '#ffffff',
                                       percent_diff() <=5 ~ '#ffffff',
                                       teamA_total() > teamB_total() ~ '#762a83',
                                       teamA_total() < teamB_total() ~ '#1b7837',),
            labelText = case_when(teamA_total() == teamB_total() ~ 'Trade is equal!',
                                  percent_diff() <=5 ~ 'Trade is ~ fair!',
                                  teamA_total() > teamB_total() ~ 'in favour of Team A',
                                  teamA_total() < teamB_total() ~ 'in favour of Team B'),
            labelFontSize = '18'
    )
    
  })
  
  output$teamA_total <- renderText({ paste("Team A total:",format(teamA_total(),big.mark = ',')) })
  output$teamB_total <- renderText({ paste("Team B total:",format(teamB_total(),big.mark = ',')) })
  
  value_container <- withTags(
    table(class = "compact row-border",
          thead(tr(
            th(style="text-align:left;","Player"),
            th(style="text-align:right;padding-right:3px;","Age"),
            th(style='text-align:right;padding-right:3px;',"Value")
          ))
    )
  )
  
  output$teamA_valuetable <- renderDT({
    teamA_values() %>%
      datatable(class = "compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                 list(className = 'dt-right', targets = -1)),
                               ordering = FALSE,
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })
  output$teamB_valuetable <- renderDT({
    teamB_values() %>%
      datatable(class = "compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                 list(`text-align` = 'right', targets = c(1,2))),
                               ordering = FALSE,
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })
  
  output$trade_plot <- render_mobile({
    
    tibble(Team = c('Team A','Team B'),
           Players = list(teamA_values(),teamB_values())) %>%
      unnest(Players) %>%
      mobile(aes(x = Team, y = Value, color = Player, adjust = stack)) %>% 
      mobile_bar() %>% 
      mobile_legend(position = 'bottom')
    
  })
  
  output$tradewizard_table <- renderDT({
    
    trade_diff <- abs(teamA_total()-teamB_total())
    
    tradebalancer_table <- values() %>%
      filter(Value<=(trade_diff*1.05),Value>=(trade_diff*0.95)) %>% 
      datatable(class = "compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                 list(className = 'dt-right', targets = -1)),
                               ordering = FALSE,
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })
  
  output$tradewizard <- renderUI({
    
    req(percent_diff()>5|is.infinite(percent_diff()))
    
    f7Card(title = "Trade Wizard", inset = TRUE,
           "These players might help balance out the trade!",
           DTOutput('tradewizard_table')
    )
  })
  
  output$results_tab <- renderUI({
    
    validate(need(input$calculate,message = "Please press Calculate to load the analysis!"))
    
    div(
      f7Card(
      div(uiOutput('trade_gauge',height = '250px'),style = 'text-align:center;'),inset= TRUE),

      f7Card(title = textOutput('teamA_total'), inset = TRUE,
             DTOutput('teamA_valuetable')
      ),
      f7Card(title = textOutput('teamB_total'), inset = TRUE,
             DTOutput('teamB_valuetable')
      ),
      f7Card(title = "Trade Plot",inset = TRUE,
             mobileOutput('trade_plot')
      ),
      uiOutput('tradewizard'),
      br(),
      br(),
      br(),
      br(),
      br()
    )
  })
  
  observeEvent(input$calculate,{
    updateF7Tabs(session, id = 'tabs', selected = 'Analysis')
  })
  
  # values tab ----
  output$values_table <- renderDT({
    values() %>%
      datatable(class = "compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               ordering = FALSE,
                               # columnDefs = list()
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })
  
  output$values <- renderUI({
    
    validate(need(input$calculate,message = "Please press Calculate to load the Values table!"))  
    
    div(
      f7Card(DTOutput('values_table')),
      br(),
      br(),
      br(),
      br(),
      br()
      )
  })
  
  # show last updated ----
  
  f7Toast(session,glue("ECR last updated {players_raw$scrape_date[[1]]}"),closeTimeout = 1000)
  
  # Save data to a sqlite file on server ----
  
  sessionID <- UUIDgenerate(1)
  
  observeEvent(input$calculate, {
    
    req(teamA_values(),teamB_values())
    
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
    
      try({
        pool_save(pool_localdb, 'calculator_log',saved_data)
        })
  })
  
} # end of server segment ----

shinyApp(ui, server) # run app ----
