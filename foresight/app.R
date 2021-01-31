suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  # library(RSQLite)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  
  # Plotting
  library(gt)
  # library(ggimage)
  # library(grid)
  # library(ggrepel)
  # library(nflfastR)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  # library(shinyWidgets)
  # library(reactable)
  library(gfonts)
  # library(DT)
  
})

source('fn_ui_desktop.R')
options(warn=0, dplyr.summarise.inform = FALSE)

# Create Functions --------------------------------------------------------

# get_players_sorted <- function(data, var) {
#   
#   data %>%
#     group_by(Name) %>% 
#     summarise(total = sum(.data[[var]], na.rm = TRUE)) %>% 
#     arrange(desc(total)) %>%
#     select(Name) %>% 
#     pull()
# }
# 
# sticky_style <- function(left = TRUE) {
#   style <- list(position = "sticky", background = "#f7f7f8", zIndex = 1)
#   if (left) {
#     style <- c(style, list(left = 0, borderRight = "1px solid #eee"))
#   } else {
#     style <- c(style, list(right = 0, borderLeft = "1px solid #eee"))
#   }
#   style
# }
# 
# colClean <- function(x) {str_to_upper(gsub("_", " ", colnames(x), fixed = TRUE))}

# Import Data -------------------------------------------------------------

setwd(here())
foresight_comps <- read_feather("foresight_comps.ftr")

# UI section --------------------------------------------------------------
ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Foresight - DynastyProcess.com",
  navbar = ui_header("Foresight App"),
  sidebar = ui_sidebar(
    menuItem('Foresight', tabName = 'comps', icon = 'chart-line'),
    menuItem('Ranks', tabName = 'ranks', icon = 'table'),
    menuItem('About', tabName = 'about', icon = 'question-circle')),
  dashboardBody(
    use_font("fira-sans-condensed", "www/css/fira-sans-condensed.css"),
    tags$style(HTML("#selectColBox {overflow:auto;}")),
    tabItems(
      tabItem(
        tabName = 'comps',
        h1('Foresight Comps', style = "padding-left:10px;"),
        box(title = "Inputs",
            status = "danger",
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  "select_season",
                  "Select Seasons:",
                  choices = rev(sort(unique(foresight_comps$main_season))),
                  selected = "2020"),
                selectizeInput(
                  "select_player",
                  "Select Players:",
                  choices = sort(unique(foresight_comps$main_name)))))
        ),
        box(
          width = 12,
          fluidRow(
            width = 12,
            gt_output("comp_table")))
      ),
      tabItem(
        tabName = "ranks",
        h1("Foresight Ranks", style = "padding-left:10px;"),
        box(title = "Inputs",
            status = "danger",
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  "select_season_rank",
                  "Select Seasons:",
                  choices = rev(sort(unique(foresight_comps$main_season))),
                  selected = "2020")))),
        box(width = 12,
            fluidRow(
              width = 12,
              gt_output("rank_table")
            ))
        ),
      tabItem(
        tabName = 'about',
        h1('About - Foresight',
           style = "padding-left:10px"),
        box(status = "danger",
            width = 12,
            fluidRow(
              column(12, includeMarkdown('about_ep.md'))
            )
        )
      )
    )
  )
)



# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  season_ranks <- reactive({
    foresight_comps %>%
      filter(main_season == input$select_season_rank) %>%
      group_by(main_player) %>%
      summarise(across(.cols = contains("baseline_y"),
                       .fns = ~mean(.x, na.rm = TRUE))
                # across(.cols = contains("baseline_y"),
                #        .fns = ~mean(if_else(.x > 0, .x, NULL), na.rm = TRUE),
                #        .names = "{.col}_upside")
                ) %>%
      ungroup() %>%
      separate("main_player",
               into = c("main_name","main_gsis_id","main_season"),sep = "\\|",
               remove = TRUE) %>%
      select(-main_gsis_id,-main_season) %>% 
      mutate(across(.cols = where(is.numeric),
                    .fns = ~round(.x,1)),
             dynasty_value = avg_fp_above_baseline_y1 +
               0.8 * avg_fp_above_baseline_y2 +
               0.64 * avg_fp_above_baseline_y3 +
               0.512 * avg_fp_above_baseline_y4 +
               0.4096 * avg_fp_above_baseline_y5,
             rank = row_number(-dynasty_value)) %>% 
      arrange(-dynasty_value)
  })
  
  output$rank_table <- render_gt(
    season_ranks() %>% 
      gt(rowname_col = "main_name") %>% 
      fmt_number(
        columns = c("avg_fp_above_baseline_y1",
                    "avg_fp_above_baseline_y2",
                    "avg_fp_above_baseline_y3",
                    "avg_fp_above_baseline_y4",
                    "avg_fp_above_baseline_y5",
                    "dynasty_value"),
        decimals = 1) %>%
      cols_move_to_start(
        columns = vars(rank)) %>% 
      cols_label(
        rank = "Rank",
        avg_fp_above_baseline_y0 = "0",
        avg_fp_above_baseline_y1 = "1",
        avg_fp_above_baseline_y2 = "2",
        avg_fp_above_baseline_y3 = "3",
        avg_fp_above_baseline_y4 = "4",
        avg_fp_above_baseline_y5 = "5",
        dynasty_value = "Weighted FPOB") %>% 
      tab_spanner(
        label = "Fantasy Points Above Top 36 Baseline",
        columns = 
          vars(
            avg_fp_above_baseline_y0,
            avg_fp_above_baseline_y1,
            avg_fp_above_baseline_y2,
            avg_fp_above_baseline_y3,
            avg_fp_above_baseline_y4,
            avg_fp_above_baseline_y5)
      ) %>% 
      data_color(
        columns = vars(dynasty_value),
        colors = scales::col_factor(
          palette = 'PRGn',
          #brewer.pal(12,'PRGn')[3:8],
          domain = NULL))
      
  )
  
  player_selected <- reactive({
    foresight_comps %>% 
      filter(main_name == input$select_player,
             main_season == input$select_season) %>%
      select(-contains("main"),
             -c(comp_gsis_id,
                comp_player,
                sim_score,
                sim_rank,
                Pos,
                avg_fp_above_baseline_y0,
                avg_fp_above_baseline_y2,
                avg_fp_above_baseline_y3,
                avg_fp_above_baseline_y4,
                avg_fp_above_baseline_y5)) %>% 
      arrange(-sim_score_adj)
  })
  
  num_cols <- reactive({
    player_selected() %>%
    select(where(is.numeric) & !contains("draft")) %>%
    colnames()
  })
  
  output$comp_table <- render_gt(
    
    player_selected() %>% 
      gt() %>% 
      data_color(
        columns = num_cols(),
        colors = scales::col_factor(
          palette = 'PRGn',
          #brewer.pal(12,'PRGn')[3:8],
          domain = NULL)) %>%
      # No Decimals
      fmt_number(
        columns = c("sim_score_adj"),
        decimals = 0) %>%
      #1 Decimal
      fmt_number(
        columns = c("player_age_eos",
                    "fpob_roll16",
                    "rec_yptpa_roll16",
                    "rec_comp_x_breakout16",
                    "total_fp_x_career",
                    "total_yd_diff_career",
                    "total_td_career",
                    "total_yd_team_x_career",
                    "avg_fp_above_baseline_y1"),
        decimals = 1) %>%
      #Percent
      fmt_percent(
        columns = c("rush_fp_x_share_breakout16",
                    "rush_fp_share_season"),
        decimals = 1) %>% 
      cols_label(
        comp_name = "Comparison",
        comp_season = "Season",
        ovr_draft_pick = "Pick",

        sim_score_adj = "Sim Score",
        avg_fp_above_baseline_y1 = "FPOB Y + 1",
        player_age_eos = "Age",
        season_games = "Games",
        
        fpob_roll16 = "FPOB 16",
        rec_yptpa_roll16 = "YPTPA 16",
        rush_fp_x_share_breakout16 = "EP Share B.O. 16",
        rec_comp_x_breakout16 = "xRec B.O. 16",
        rec_ay_roll16 = "Rec AY 16",
        
        total_fp_x_career = "EP/Game",
        games_above_base_todate = "Games Above",
        total_yd_diff_career = "Yards over xYards",
        total_td_career = "TD/Game",
        total_yd_team_x_career = "Team xYard",
        
        rush_fp_share_season = "Rush FP Share",
        total_td_season = "Total TD Season") %>% 
    tab_spanner(
      label = "Career",
      columns = 
        vars(
          total_fp_x_career,
          games_above_base_todate,
          total_yd_diff_career,
          total_td_career,
          total_yd_team_x_career
        )
    )
  )
  

}

# thematic_shiny(font = "auto")
shinyApp(ui, server)
