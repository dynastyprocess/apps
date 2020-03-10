library(shiny)
library(shinyMobile)
library(tidyverse)
library(DBI)
library(tidyverse)
library(glue)
library(sever)
library(arrow)
library(DT)
library(joker)
# library(hrbrthemes)

players <- read_parquet("players.pdata")
rookies <- read_parquet("pick_values.pdata")

# create_manifest(path = here::here(),
#                 name = "DynastyProcess Trade Calculator",
#                 shortName = "DP Calc",
#                 description = "The DynastyProcess Trade Calculator helps you analyze trades with unparalleled control and customization.",
#                 lang = "en-US",startUrl = "https://apps.dynastyprocess.com/calc",display = "standalone",
#                 background_color = "black",theme_color = "black",
#                 icon = data.frame(src = c("www/128x128.png"
#                                           ,'www/180x180.png'
#                                           ,'www/192x192.png'
#                                           ,'www/512x512.png'
#                                           ,'www/32x32.png'
#                                           ,'www/16x16.png'),
#                                   sizes = c('128x128'
#                                             ,'180x180'
#                                             ,'192x192'
#                                             ,'512x512'
#                                             ,'32x32'
#                                             ,'16x16'),
#                                   type  = c('image/png'
#                                             ,'image/png'
#                                             ,'image/png'
#                                             ,'image/png'
#                                             ,'image/png'
#                                             ,'image/png'))
#                 
#                 
#                   )

ui <- f7Page(
  title = "DynastyProcess Trade Calculator",
  dark_mode = FALSE,
  manifest = "www/manifest.json",
  favicon = "www/favicon.ico",
  icon = 'www/128x128.png',
  init = f7Init(
    skin = 'md',
    theme = 'dark',
    color = 'pink',
    tapHold = FALSE
  ),
  f7TabLayout(
    use_sever(),
    tags$head(tags$style(HTML("
table td, table tr, table th{
background: transparent !important;
}
                              "))),
    navbar = f7Navbar(title = 'DynastyProcess.com',
                      subtitle = "Trade Calculator"
                      # left_panel = TRUE,
                      # bigger = TRUE
                      # hairline = FALSE
    ),
    panels = tagList(f7Panel(
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
    f7Tabs(
      # swipeable = TRUE,
      # animated = TRUE,
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
                               selected = '12 teams'
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
        br()
      ),
      f7Tab( # analysis tab ----
        tabName = "Analysis",
        icon = f7Icon('graph_circle_fill',old = FALSE),
        h1("Trade Analysis",style = 'text-align:center;'),
        f7Card(div(f7Gauge('Score',type = 'semicircle', value = 45,
                           borderBgColor = 'white',
                           borderColor = '#ff2d55',
                           valueText = '10%',
                           valueTextColor = 'white',
                           labelText = "in favour of Team B",
                           labelTextColor = 'white',
                           labelFontSize = '20',
                           labelFontWeight = 'bold'
        ),style = 'text-align:center;'),inset = TRUE),
        
        f7Card(title = "Team A total: 24,400", inset = TRUE,
               DTOutput('teamA_values')
        ),
        f7Card(title = "Team B total: 24,400", inset = TRUE,
               DTOutput('teamB_values')
        ),
        br(),
        br()
        
      ),
      f7Tab(tabName = "Values", # values tab ----
            icon = f7Icon('square_favorites_fill',old = FALSE),
            h1('Values - Quick Reference',style = 'text-align:center;'),
            f7Card(DTOutput('values'))
      ),
      f7Tab(tabName = "About", # about tab ----
        icon = f7Icon('info_circle_fill',old = FALSE)
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

  # Get data from database ----
  # aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")
  
  # players <- dbGetQuery(aws_db,"
  # SELECT DISTINCT
  # 
  # mfl_id,
  # fantasypros_id,
  # name,
  # position,
  # age,
  # COALESCE(fp_ecr.ecr,400) as ecr,
  # dp_playerids.team
  # 
  # FROM dp_playerids
  # 
  # LEFT JOIN fp_ecr ON dp_playerids.fantasypros_id = fp_ecr.id
  # AND fp_ecr.ecr_type = 'do'
  # AND fp_ecr.scrape_date = (SELECT scrape_date FROM fp_ecr ORDER BY scrape_date desc limit 1)
  # 
  # WHERE position in ('QB','RB','WR','TE')
  # ORDER BY ecr
  # 
  #                     ")
  
  # write_parquet(players,"players.pdata")
  
  # pick_values <- dbGetQuery(aws_db,"SELECT * FROM dp_rookiepickvalues")
  
  # write_parquet(pick_values,"rookie_values.pdata")
  
  # # dbDisconnect(aws_db)
  # players <- read_parquet("players.pdata")
  # rookies <- read_parquet("pick_values.pdata")
  # Calculate player and pick values based on the slider inputs ----
  
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
  
  # Results tab
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
    players %>% 
      select(Name = name, Pos = position, Team = team, Age = age, ECR = ecr) %>% 
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
