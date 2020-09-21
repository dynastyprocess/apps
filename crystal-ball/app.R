
suppressPackageStartupMessages({

  # Data import
  library(ffscrapr)
  library(httr)
  library(jsonlite)
  library(here)
  library(arrow)

  # library(DBI)
  # library(RSQLite)

  # Data manipulation
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(rlang)

  # Plotting
  # library(ggimage)
  # library(grid)
  # library(ggrepel)
  # library(nflfastR)

  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(reactable)
  library(gfonts)
  # library(DT)
})

source("fn_ui.R")
options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Crystal Ball - DynastyProcess.com",
  navbar = ui_header("Crystal Ball - DynastyProcess.com"),
  sidebar = ui_sidebar(
    menuItem("Crystal Ball", tabName = "crystalball", icon = "hat-wizard"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch")
  ),
  body = dashboardBody(
    use_font("fira-sans-condensed", "www/css/fira-sans-condensed.css"),
    tabItems(
      tabItem(
        tabName = "crystalball",
        header_box(),
        league_select(),
        uiOutput("season_projections"),
        uiOutput("weekly_schedule")
        )
    )
  )
)


server <- function(input, output, session) {

  output$league_authbox <- renderUI({

    switch(input$platform,
           "MFL" = league_authbox.mfl(),
           "Sleeper" = league_authbox.sleeper(),
           "ESPN" = league_authbox.espn())
  })


  user_obj <- reactiveValues()

  observeEvent({input$platform;input$league_id;input$user_name;input$password},{
               user_obj$platform <- input$platform
               user_obj$league_id <- input$league_id
               user_obj$user_name <- input$user_name
               user_obj$password <- input$password
               })

  user_leagues <- eventReactive(
    input$load_user,{

      req(input$platform %in% c("MFL","Sleeper"))
      user_leagues.ffscrapr(user_obj)

    })

  output$team_select <- renderUI({

    switch(input$platform,
           "MFL" = team_select.ffscrapr(user_leagues()),
           "Sleeper" = team_select.ffscrapr(user_leagues()),
           "ESPN" = team_select.espn())
  })

  # observe(user_leagues())

  # output$team_select <- renderUI({

    # user_leagues %>%
    #   select(league_name,
    #          league_id,
    #          franchise_name) %>%
    #   mutate(Select = map_chr(league_id,~actionButton(inputId = paste0('leagueid_',.x), "Select") %>% as.character())) %>%
    #   clean_names("upper_camel") %>%
    # reactable()

  # })

}

shinyApp(ui, server)
