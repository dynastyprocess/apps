# SETUP  ----------------------------------------------------------

suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
  library(ffsimulator)
  library(tidyverse)
  library(gt)
  library(RColorBrewer)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  
})

options(warn=0, dplyr.summarise.inform = FALSE)

# DATA PREP --------------------------------------------------------------


# UI section --------------------------------------------------------------
ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Start/Sit Guide - DynastyProcess.com",
  # navbar = ui_header("Start/Sit App"),
  dashboardBody(
    # use_font("fira-sans-condensed", "www/css/fira-sans-condensed.css"),
    tags$style(HTML("#selectColBox {overflow:auto;}")),
    tabItems(
      tabItem(tabName = 'weekly',
              h1('Roster Breakdown', style = "padding-left:10px;"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           selectizeInput("select_platform",
                                          "Select Playform:",
                                          choices = c("mfl","sleeper","fleaflicker","espn"),
                                          multiple = FALSE,
                                          selected = "fleaflicker")
                    ),
                    column(width = 4,
                           textInput("league_id", label = "Enter League ID", value = 312861)
                           # actionButton("load_data",
                           #              "Load League",
                           #              icon = icon("list-ol"),
                           #              class = "btn-success")
                    ),
                    column(width = 4,
                           selectizeInput("select_team",
                                          "Select Team:",
                                          choices = c(),
                                          multiple = FALSE,
                                          selected = NULL)))))),
    box(
      width = 12,
      fluidRow(width = 12,
               gt_output("team_table"))))
)


# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  
  league_conn <- reactiveVal()
  rosters <- reactiveVal()
  
  #react on enter button
  observeEvent({input$league_id},{
    league_conn(load_conn(input$select_platform, input$league_id))
    
    rosters(combine_sources(league_conn(), input$select_platform))
    
    })
  
  observeEvent({input$league_id},{
    updateSelectizeInput(session, 'select_team',
                         choices = rosters() %>% pull(franchise_name) %>% distinct(),
                         selected = rosters() %>% sample_n(1) %>% pull(franchise_name))
    
  })

  output$team_table <- render_gt({
    req(input$league_id)
    
    rosters() %>% 
      filter(franchise_name == input$select_team) %>% 
      arrange(position, -projected_points, -ecr) %>% 
      gt() %>%
      tab_header(title = "Start/Sit Guide") %>%
      cols_label(franchise_name = "Franchise Name",
                 player_name = "Player",
                 position = "Position",
                 team = "Team",
                 ovr_rank = "Overall",
                 ecr = "Consensus",
                 best = "Best",
                 worst = "Worst",
                 projected_points = "Projected Points") %>% 
      tab_spanner(label = "Rank",
                  columns = c(ovr_rank, ecr, best, worst)) %>% 
      data_color(
        columns = c(projected_points),
        colors = scales::col_factor(
          brewer.pal(11,'PRGn')[3:8],
          domain = NULL
        ))%>% 
      data_color(
        columns = c(ecr),
        colors = scales::col_factor(
          brewer.pal(11,'PRGn')[8:3],
          domain = NULL
        ))
  })
  
}

shinyApp(ui, server)