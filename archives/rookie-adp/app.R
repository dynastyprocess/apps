#### Collection Form ####
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

source('fn_ui.R')

ui <- dashboardPage(
  sidebar_collapsed = FALSE,
  navbar = ui_header(),
  sidebar = ui_sidebar(
    menuItem('Rookie ADP',tabName = 'community_adp',icon = 'user-friends'),
    menuItem('More by DynastyProcess',tabName = 'about',icon = 'rocket')
  ),
  body = ui_body(
    tabItem(
      tabName = 'community_adp',
      fluidRow(
      box(
        title = 'About', width = 4, status = 'danger',
        "This is a rookie ADP compilation based on community contributions from the r/DynastyFF, TDM Discord, and Twitter communities. 
        If you're interested in adding your league, please fill out the form here!"
        ),
      box(
        title = 'Add Your LeagueID Here!', width = 8, status = 'info',
        footer = actionButton('submit','Submit',class = 'btn-info',width ='100%'),
        fluidRow(
          column(
            width = 3,
            pickerInput(
              'league_platform',
              NULL,
              width = '100%',
              choices = c('MFL', 'Sleeper'),
              options = list(title = 'League Platform')
            ),
          ),
          column(
            width = 3,
            pickerInput(
              'community',NULL,width = '100%',
              choices = c('r/DynastyFF', 'TDM Discord', 'Twitter', 'Other'),
              options = list(title = 'Community (optional)')
            ),
          ),
          column(width = 6,
                 uiOutput('mfl_input'),
                 uiOutput('sleeper_input'))
          
      ))),
      fluidRow(box(
        title = 'ADP Results',
        width = 12,
        status = 'gray-dark'
      )), 

  ) 
  
)
)
server <- function(input, output, session) {
  
  sever_joke()
  
  # Render League-Specific Inputs ----
  output$sleeper_input <- renderUI({
    req(input$league_platform == 'Sleeper')
    div(
      textInput('sleeper_draftid', label = NULL,placeholder = "Sleeper Draft ID",width = '100%'),
      helpText(em("This is the ID from the Draftboard/Results page itself, not your league homepage!"))
    )
  })
  output$mfl_input <- renderUI({
    req(input$league_platform == 'MFL')
    div(
      textInput('mfl_leagueid', label = NULL,placeholder = "MFL League ID",width = '100%')
    )
  })
  
  # Validate inputs ----
  
  observeEvent(input$submit,{
    
  })
  
}

shinyApp(ui, server)
