
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
  library(RColorBrewer)
  library(waiter)
  library(sever)

  # Data output
  library(writexl)

})

source("functions.R")
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
    includeCSS("dp.css"),
    use_waiter(),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "crystalball",
        box_about(),
        box_leagueselect(),
        uiOutput("season_projections"),
        uiOutput("weekly_schedule"),
        uiOutput("download_button"),
        # actionButton("debug", "debug")
      )
    )
  )
)


server <- function(input, output, session) {

  sever_dp()

  waiter_teamselect <- Waiter$new(
    id = "column_team_select",
    html = spin_dots(),
    color = transparent(0.5)
  )

  output$league_authbox <- renderUI({

      switch(input$platform,
             "MFL" = league_auth.mfl(),
             "Sleeper" = league_auth.sleeper()
      )

  })

  user_obj <- reactiveValues()

  observeEvent(
    {
      input$platform
      input$league_id
      input$league_select
      input$user_name
      input$password
    },
    {
      user_obj$platform <- input$platform
      user_obj$season <- format(Sys.Date(), "%Y")
      user_obj$league_id <- input$league_id
      user_obj$league_id <- input$league_select
      user_obj$user_name <- input$user_name
      user_obj$password <- input$password
    }
  )

  user_leagues <- eventReactive(
    input$load_user,
    {
      req(input$platform %in% c("MFL", "Sleeper"))

      waiter_teamselect$show()
      on.exit(waiter_teamselect$hide())

      x <- tryCatch({
      user_leagues.ffscrapr(user_obj)
      },
      error = function(e){
        showModal(
          modalDialog(
            title = "Oh no, ran into an error!",
            glue("Couldn't load leagues for {user_obj$platform} user {user_obj$user_name}.")))
        return(NULL)
      })

      return(x)
    }
  )

  output$team_select <- renderUI({

    req(user_leagues())

    switch(input$platform,
      "MFL" = team_select.ffscrapr(user_leagues()),
      "Sleeper" = team_select.ffscrapr(user_leagues()),
      "ESPN" = team_select.espn()
    )
  })

  loaded_data <- reactiveValues()

  observeEvent(
    user_obj$league_id,
    {
      waiter_show(
        html = spin_dots(),
        color = transparent(0.5))

      on.exit({
        waiter_hide()
        bs4Dash::updatebs4Card('box_leagueselect',session,'toggle')
        })

      loaded_data <-
        tryCatch(
          load_data.ffscrapr(user_obj, loaded_data),
          error = function(e) {
            showModal(
              modalDialog(
                title = "Oh no, ran into an error!",
                glue("Couldn't load data for {user_obj$platform} league {user_obj$league_id}.")))
              }
        )

    }
  )

  output$season_projections <- renderUI({
    req(loaded_data$standings_forecast)

    season_projection(loaded_data)

  })

  output$weekly_schedule <- renderUI({
    req(loaded_data$schedule_pivot)

    weekly_schedule(loaded_data)

  })

  output$download_button <- renderUI({
    req(loaded_data$standings_forecast)

    div(downloadButton('download',"Download Data!"), style = 'text-align:center;')
  })

  output$download <- downloadHandler(
    filename = glue::glue("DP_CrystalBall_{Sys.Date()}.xlsx"),
    content = function(file){
      write_xlsx(
        reactiveValuesToList(loaded_data),
        path = file,
        format_headers = TRUE
      )
    }
  )

  #### DEBUG ####
  observeEvent(input$debug, browser())
}

shinyApp(ui, server)
