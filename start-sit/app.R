# SETUP  ----------------------------------------------------------

suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
  library(ffsimulator)
  library(nflreadr)
  library(tidyverse)
  library(gt)
  library(gtExtras)
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
                                          choices = c("MFL" = "mfl",
                                                      "Sleeper" = "sleeper",
                                                      "Fleaflicker" = "fleaflicker",
                                                      "ESPN" = "espn"),
                                          multiple = FALSE,
                                          selected = "fleaflicker")
                    ),
                    column(width = 4,
                           textInput("league_id", label = "Enter League ID", value = 312861)),
                    column(width = 4,
                           actionButton("load_data",
                                        "Load League",
                                        class = "btn-success")))))),
    box(width = 12,
        column(width = 4,
               selectizeInput("select_team",
                              "Select Team:",
                              choices = c(),
                              multiple = FALSE,
                              selected = NULL)),
        fluidRow(width = 12,
                 gt_output("team_table")))
  )
)


# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  
  league_conn <- reactiveVal()
  rosters <- reactiveVal()
  
  #react on enter button
  observeEvent({input$load_data},{
    league_conn(load_conn(input$select_platform, input$league_id))
    
    rosters(combine_sources(league_conn(), input$select_platform))
    
    updateSelectizeInput(session, 'select_team',
                         choices = rosters() %>% pull(franchise_name) %>% unique(),
                         selected = rosters() %>%
                           filter(franchise_name != "Free Agents") %>% 
                           sample_n(1) %>%
                           pull(franchise_name))
    
  })

  output$team_table <- render_gt({
    req(input$select_team)
    
    rosters() %>% 
      filter(franchise_name == input$select_team) %>% 
      arrange(position, -projected_points, -ecr) %>%
      select(-c(franchise_name, team)) %>% 
      gt() %>%
      tab_header(title = "Start/Sit Guide") %>%
      cols_label(player_name = "Player",
                 player_image_url = "",
                 position = "Position",
                 team_wordmark = "Team",
                 ovr_rank = "Overall",
                 pos_rank = "Positional",
                 ecr = "Consensus",
                 best = "Best",
                 worst = "Worst",
                 projected_points = "Projection",
                 roster_pct = "Roster %",
                 practice_status = "Practice",
                 report_status = "Report",
                 snap_data = "Snap Trend") %>%
      fmt_missing(columns = c(pos_rank, ovr_rank, ecr, best, worst, player_image_url, team_wordmark),
                  missing_text = "") %>% 
      fmt_pad_num(columns = c(ecr, projected_points), nsmall = 1, gfont = "Fira Mono") %>%
      fmt_percent(columns = roster_pct, decimals = 1) %>% 
      gt_img_rows(columns = c(player_image_url,team_wordmark), height = 40) %>% 
      tab_spanner(label = "Rank", columns = c(ovr_rank, pos_rank, ecr, best, worst)) %>%
      tab_spanner(label = "Injury Report", columns = c(practice_status, report_status)) %>%
      gt_hulk_col_numeric(columns = projected_points) %>% 
      gt_hulk_col_numeric(columns = ecr, reverse = TRUE) %>%
      gt_sparkline(snap_data) %>% 
      cols_move(columns = c(player_image_url, position, team_wordmark,
                            ovr_rank, pos_rank, ecr, best, worst,
                            projected_points, practice_status, report_status,
                            snap_data),
                after = player_name) %>% 
      cols_align(columns = team_wordmark, align = "center")  %>%
      tab_style(
        style = cell_text(font = google_font("Fira Mono"), weight = 800),
        locations = cells_title(groups = "title")
      ) %>%
      tab_style(
        style = cell_text(
          size = px(16),
          color = "darkgrey",
          font = google_font("Fira Mono"),
          transform = "uppercase"),
        locations = cells_column_labels()) %>%
    tab_style(
      style = cell_text(
        size = px(20),
        color = "darkgrey",
        font = google_font("Fira Mono"),
        transform = "uppercase"),
      locations = cells_column_spanners()) %>%
      tab_style(
        style = cell_text(font = google_font("Fira Mono"), weight =  400),
        locations = cells_body())
  })
  
}

shinyApp(ui, server)