library(shiny)
library(shinyMobile)
library(tidyverse)
library(glue)
library(sever)
library(arrow)
library(DT)
library(joker)
library(lubridate)
library(stringr)
# library(hrbrthemes)

# Read data from local ----
players <- read_parquet("players.pdata")
rookies_raw <- read_parquet("pick_values.pdata")

ui <- f7Page( # f7Page setup and Init Options ----
  title = "DynastyProcess Trade Calculator",
  dark_mode = FALSE,
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
    tags$head(tags$style(HTML("
table td, table tr, table th{
background: transparent !important;
}
                              "))),
    navbar = f7Navbar(title = "DynastyProcess.com", # Navbar ----
                      subtitle = "Trade Calculator",
                      # left_panel = TRUE,
                      # bigger = TRUE
                      hairline = TRUE
    ),
    panels = tagList(f7Panel( # Sidebar panels (currently just a help panel) ---- 
      inputId = "panel_left",
      title = "Trade Calculator Help",
      side = "left",
      theme = "light",
      effect = "cover",
      h3("Inputs",style = "text-align:center;"),
      f7Accordion(
        f7AccordionItem(title = "QB Type",
                        f7Block(inset = TRUE,
                                "Toggles between the base FantasyPros 1QB values and our algorithm-generated 2QB values.")),
        f7AccordionItem(title = "League Size",
                        f7Block("Renames rookie and startup selections. Does not adjust values, you'll need to tweak the Valuation Factor for that."),inset = TRUE),
        f7AccordionItem(title = "Display Mode",
                        f7Block(inset = TRUE,"Adds labeling so that you can assess the value of startup pick trades, and can also include placeholder picks for a separate rookie draft.")),
        f7AccordionItem(title = "Valuation Factor",
                        f7Block(inset = TRUE,"Tunes how star players are valued relative to bench players, so that you can tweak values for league size/scoring, market preferences, and personal strategy.")),
        f7AccordionItem(title = "Rookie Pick Optimism",
                        f7Block(inset = TRUE,"Adjusts between our Perfect Knowledge and Hit Rate algorithms for valuing rookie picks.")),
        f7AccordionItem(title = "Future Pick Factor",
                        f7Block(inset = TRUE,"Adjusts value of future rookie picks."))
      )
    )
    ),
    appbar = NULL,
    f7Tabs( # Main tabs ----
      # swipeable = TRUE,
      # animated = FALSE,
      id = 'tabs',
      f7Tab( # input tab ----
        tabName = "Inputs",
        icon = f7Icon('wand_stars',old = FALSE),
        active = TRUE,
        # img()
        h1("Main Inputs", style = "text-align:center"),
        
        uiOutput('teamAinput'),
        uiOutput('teamA_list'),
        uiOutput('teamBinput'),
        uiOutput('teamB_list'),
        
        f7Button('calculate',"Calculate!",
                 # rounded = TRUE, 
                 shadow = TRUE),
        f7SmartSelect('calc_type',
                      label = "Trade Details", 
                      smart = FALSE,
                      choices = c("I'm considering this trade",
                                  "I've received this trade offer",
                                  "I've completed this trade")),
        f7Card(title = 'Customize Value Settings',
               f7Row(
                 f7SmartSelect('qb_type',label = 'QB Type',choices = c('1QB','2QB/SF')),
                 f7SmartSelect('teams',
                               label = 'Teams',
                               choices = glue("{seq(6,24,2)} teams"),
                               selected = "12 teams"
                 ),
                 f7SmartSelect('draft_type',
                               label = "Display Mode",
                               choices = c('Normal',
                                           'Startup (Players & Picks)',
                                           'Startup (Players Only)'))
                 
               ),
               f7Slider('value_factor',"Valuation Factor",min = 0.0210,max = 0.0260,value = 0.0235,step = 0.0005,
                        labels = tagList(f7Icon("square_stack_3d_up_fill", old = FALSE),
                                         f7Icon("star_circle_fill",old = FALSE))
               ),
               f7Slider('rookie_optimism','Rookie Optimism',
                        min = 0,max = 100,value = 80,step = 5,
                        labels = tagList(
                          f7Icon("bolt_slash_fill",old = FALSE),
                          f7Icon("bolt_fill",old = FALSE)
                        )),
               f7Slider('future_factor','Future Pick Value',
                        min = 65,max = 95,value = 80,step = 5,
                        labels = tagList(
                          f7Icon("play_fill",old = FALSE),
                          f7Icon("forward_fill",old = FALSE)
                        )),
               br(),
               f7Button('toggle_inputhelp',label = "Help",
                        shadow = TRUE, 
                        size = 'small',
                        rounded = TRUE)
        ),
        br(),
        br(),
        br(),
        br(),
        br()
      ),
      f7Tab( # analysis tab ----
        tabName = "Analysis",
        icon = f7Icon('graph_circle_fill',old = FALSE),
        h1("Trade Analysis",style = 'text-align:center;'),
        f7Card(div(f7Gauge('Score',type = 'semicircle', value = 30,
                           borderBgColor = '#1b7837',
                           borderColor = '#762a83',
                           valueText = '20%',
                           valueTextColor = '#1b7837',
                           labelText = "in favour of Team B",
                           # labelTextColor = 'white',
                           labelFontSize = '18',
                           # labelFontWeight = 'medium'
        ),style = 'text-align:center;'),inset = TRUE),
        
        f7Card(title = "Team A total: 24,400", inset = TRUE,
               DTOutput('teamA_values')
        ),
        f7Card(title = "Team B total: 24,400", inset = TRUE,
               DTOutput('teamB_values')
        ),
        br(),
        br(),
        br(),
        br()
        
      ),
      f7Tab(tabName = "Values", # values tab ----
            icon = f7Icon('square_favorites_fill',old = FALSE),
            h1('Values - Quick Reference',style = 'text-align:center;'),
            f7Card(DTOutput('values')),
            br(),
            br(),
            br(),
            br(),
      ),
      f7Tab(tabName = "About", # about tab ----
        icon = f7Icon('info_circle_fill',old = FALSE),
        br(),
        # h1('About',style = "text-align:center;"),
        # f7Row(f7Col(img(src = "icons/128x128.png")),
                 # f7Col(h1("DynastyProcess.com")),
              # f7Col()),
        div(img(src = 'icons/128x128.png'),style = 'text-align:center;'),
        br(),
        f7Card(title = "About",
        includeMarkdown('about.md')
        ),
        br(),
        f7Card(title = "More by DynastyProcess:"),
        br(),
        f7Card(title = "Popular Players")
      )
    )
  )) # end of UI tab ----

server <- function(input, output, session) { # start of server ----
  sever({
    tagList(
      h1("Disconnected"),
      p(em(randomjoke())),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    )
    bg_color = "#000"
  })

  # Calculate player and pick values based on the slider inputs ----
  
  rookie_optimism <- 80/100  
  futurerookie_factor <- 80/100
  value_factor <- 0.0235
  
  calculate_value <- function(df,value_factor){
    df %>% 
      mutate(value = 10500 * exp(-value_factor * ecr))
  } 
  
  label_currentpicks <- function(df,leaguesize) {
    l_s <- leaguesize + 0.001
    
    df %>% 
      mutate(
        season = year(as_date(update_date)),
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
  
  calc_currentrookies <- function(df,rookie_optimism){
    r_o <- rookie_optimism/100
    df %>% mutate(ecr = r_o*high_model + (1-r_o)*low_model)
  }
  
  pickvalues <- reactive({
    rookies_raw %>% 
    calc_currentrookies(input$rookie_optimism) %>% 
    label_currentpicks(parse_number(input$teams)) %>% 
    calculate_value(input$value_factor) %>% 
    add_futurepicks(input$future_factor,parse_number(input$teams)) %>% 
    select(player = pick_label,position,value)
  })
  # Render team input fields ----
  
  output$teamAinput <- renderUI({
    f7AutoComplete('players_teamA',
                   label = "Add Players to Team A",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = glue("{players$name}, {players$position} {players$team}"),
                   value = glue("{players$name}, {players$position} {players$team}")[sample(1:32,1)])})
  
  output$teamBinput <- renderUI({
    f7AutoComplete('players_teamB',
                   label = "Add Players to Team B",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = glue("{players$name}, {players$position} {players$team}"),
                   value = glue("{players$name}, {players$position} {players$team}")[sample(1:32,1)])})
  
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
  
  observeEvent(input$toggle_inputhelp, {
    updateF7Panel(inputId = "panel_left", session = session)
  })
  

  # Results tab ----
  output$teamA_values <- renderDT({
    players %>% 
      mutate(Player = glue("{name}, {position} {team}")) %>% 
      filter(Player %in% input$players_teamA) %>% 
      select(Player, Age = age, ECR = ecr) %>%
      datatable(class = "compact row-border",
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
  
  output$teamB_values <- renderDT({
    players %>% 
      mutate(Player = glue("{name}, {position} {team}")) %>% 
      filter(Player %in% input$players_teamB) %>% 
      select(Player, Age = age, ECR = ecr) %>%
      datatable(class = "compact row-border",
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
  
  # values tab ----
  output$values <- renderDT({
    pickvalues() %>% 
      # select(Name = name, Pos = position, Team = team, Age = age, ECR = ecr) %>% 
      datatable(class = "compact row-border",
                selection = 'none',
                options = list(searching = FALSE,
                               ordering = FALSE,
                               # columnDefs = list()
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })
  
  
  
  
} # end of server segment ----

shinyApp(ui, server) # run app ----
