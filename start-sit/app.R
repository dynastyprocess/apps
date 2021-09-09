# SETUP  ----------------------------------------------------------

suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
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

get_projections <- function(pos){fp_projections(page = pos, sport = "nfl", year = 2021, week = 1)}

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
                                          selected = NULL)
                    ),
                    column(width = 4,
                           textInput("league_id", label = "Enter League ID")
                    ),
                    column(width = 4,
                           selectizeInput("select_team",
                                          "Select Team:",
                                          choices = c(),
                                          multiple = FALSE,
                                          selected = NULL)
                    )
                  )
              )
      )
    ),
    box(
      width = 12,
      fluidRow(width = 12,
               gt_output("team_table")))
  )
)


# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  league_conn <- reactive({ ff_connect(platform = input$select_platform, league_id = input$league_id, season = 2021) })
  
  league_settings <- reactive({ ff_league(league_conn()) })
  
  league_franchises <-
    reactive({
      req(input$league_id)
      ff_franchises(league_conn()) })
  
  observeEvent({input$league_id},{
    updateSelectizeInput(session, 'select_team',
                         choices = league_franchises()$franchise_name,
                         selected = league_franchises() %>% sample_n(1) %>% pull(franchise_name))
  })
  
  projections_df <- 
    reactive({
      tibble(position = c("qb","rb","wr","te")) %>% 
        mutate(projections = map(position, get_projections),
               position = str_to_upper(position)) %>% 
        unnest(projections) %>% 
        pivot_longer(cols = where(is.numeric))
    })
  
  stat_mapping <-
    reactive({
      ffscrapr::nflfastr_stat_mapping %>%
        mutate(stat_name = case_when(nflfastr_event == "passing_yards" ~ "passing_yds",
                                     nflfastr_event == "completions" ~ "passing_cmp",
                                     nflfastr_event == "attempts" ~ "passing_att",
                                     nflfastr_event == "interceptions" ~ "passing_ints",
                                     nflfastr_event == "carries" ~ "rushing_att",
                                     nflfastr_event == "rushing_yards" ~ "rushing_yds",
                                     str_detect(nflfastr_event, "fumbles_lost")  ~ "misc_fl",
                                     nflfastr_event == "receptions" ~ "receiving_rec",
                                     nflfastr_event == "receiving_yards" ~ "receiving_yds",
                                     TRUE ~ nflfastr_event)) %>% 
        filter(platform == "sleeper") %>% 
        select(-nflfastr_event) %>% 
        distinct()
    })

  join_rules <- 
    reactive({
      projections_df() %>% 
        inner_join(stat_mapping(), by = c("name"="stat_name"), na_matches ="never") %>% 
        left_join(ff_scoring(league_conn()), by = c("ff_event"="event","position"="pos"), na_matches ="never") %>% 
        mutate(projected_points = value*points) %>% 
        group_by(position, fantasypros_id, player_name, team) %>% 
        summarise(projected_points = sum(projected_points, na.rm = TRUE))
    })
  
  rankings_df <- 
    reactive({
      if(league_settings()$qb_type == "2QB/SF" & str_detect(league_settings()$scoring_flags,"1_ppr")) {
        fp_rankings(page = "ppr-superflex", sport = "nfl")
      } else if (league_settings()$qb_type == "2QB/SF") {
        fp_rankings(page = "superflex", sport = "nfl")
      } else if (str_detect(league_settings()$scoring_flags,"1_ppr")) {
        fp_rankings(page = "ppr-flex", sport = "nfl") %>% 
          bind_rows(fp_rankings(page = "qb", sport = "nfl"))
      } else {fp_rankings(page = "flex", sport = "nfl") %>% 
          bind_rows(fp_rankings(page = "qb", sport = "nfl"))}
    })
  
  rosters <-
    reactive({
      rankings_df() %>% 
        left_join(select(ffscrapr::dp_playerids(), fantasypros_id, sleeper_id), by = "fantasypros_id") %>%
        left_join(join_rules(), by = "fantasypros_id") %>% 
        left_join(ff_rosters(league_conn()), by = c("sleeper_id"="player_id")) %>% 
        transmute(franchise_name,
                  player_name,
                  position = factor(position, levels = c("QB","RB","WR","TE"), ordered = TRUE),
                  team,
                  rank,
                  ecr,
                  best,
                  worst,
                  projected_points)
    })
  
  # league_starters <- ff_starter_positions(league_conn)
  
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
                 rank = "Position",
                 ecr = "Consensus",
                 best = "Best",
                 worst = "Worst",
                 projected_points = "Projected Points") %>% 
      tab_spanner(label = "Rank",
                  columns = c(rank, ecr, best, worst)) %>% 
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