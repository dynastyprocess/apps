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
  library(RColorBrewer)
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
                       .fns = ~mean(if_else(.x > 0, .x, 0), na.rm = TRUE),
                       .names = "{.col}_upside"),
                across(.cols = contains("baseline_y") & !contains("upside"),
                       .fns = ~sum(if_else(.x > 0, 1, 0), na.rm = TRUE),
                       .names = "{.col}_upside_counts"),
                
                across(.cols = contains("baseline_y") & !contains("upside"),
                       .fns = ~mean(.x, na.rm = TRUE))) %>%
      ungroup() %>%
      separate("main_player",
               into = c("main_name","main_gsis_id","main_season"),sep = "\\|",
               remove = TRUE) %>%
      select(-main_gsis_id,-main_season) %>% 
      mutate(dynasty_value = avg_fp_above_baseline_y1 +
               0.8 * avg_fp_above_baseline_y2 +
               0.64 * avg_fp_above_baseline_y3,
             dynasty_value_upside = avg_fp_above_baseline_y1_upside +
               0.8 * avg_fp_above_baseline_y2_upside +
               0.64 * avg_fp_above_baseline_y3_upside,
             diff = dynasty_value_upside - dynasty_value,
             rank = row_number(-dynasty_value_upside),
             across(.cols = where(is.numeric),
                    .fns = ~round(.x,1))) %>%
      arrange(-dynasty_value_upside) %>% view() %>% 
      select(main_name,
             rank,
             avg_fp_above_baseline_y0_upside,
             avg_fp_above_baseline_y1_upside,
             avg_fp_above_baseline_y2_upside,
             avg_fp_above_baseline_y3_upside,
             dynasty_value_upside)

      
  })
  
  output$rank_table <- render_gt(
    season_ranks() %>% 
      gt(rowname_col = "main_name") %>% 
      fmt_number(
        columns = c("avg_fp_above_baseline_y1_upside",
                    "avg_fp_above_baseline_y2_upside",
                    "avg_fp_above_baseline_y3_upside",
                    "dynasty_value_upside"),
        decimals = 1) %>%
      cols_move_to_start(
        columns = vars(rank)) %>% 
      cols_label(
        rank = "Rank",
        avg_fp_above_baseline_y0_upside = "0",
        avg_fp_above_baseline_y1_upside = "1",
        avg_fp_above_baseline_y2_upside = "2",
        avg_fp_above_baseline_y3_upside = "3",
        dynasty_value_upside = "Weighted FPOB") %>% 
      tab_spanner(
        label = "Fantasy Points Above Baseline Year + N",
        columns = 
          vars(
            avg_fp_above_baseline_y0_upside,
            avg_fp_above_baseline_y1_upside,
            avg_fp_above_baseline_y2_upside,
            avg_fp_above_baseline_y3_upside)
      ) %>% 
      data_color(
        columns = vars(dynasty_value_upside),
        colors = scales::col_factor(
          palette = brewer.pal(12,'PRGn')[3:8],
          domain = NULL))
  )
  
  player_selected <- reactive({
    foresight_comps %>% 
      filter(main_name == input$select_player,
             main_season == input$select_season) %>%
      arrange(-sim_score_adj)
  })
  
  player_selected_pos <- reactive({
    foresight_comps %>% 
      filter(main_name == input$select_player,
             main_season == input$select_season,
             main_player == comp_player) %>%
      pull(Pos)
  })
  
  create_gt <- function(pos, df) {
    
    if(pos == "RB") {
      df %>% 
      select(comp_name,
             comp_season,
             ovr_draft_pick,
             sim_score_adj,
             avg_fp_above_baseline_y1,
             player_age_eos,
             fpob_roll16,
             rec_yptpa_roll16,
             rush_fp_x_share_breakout16,
             rec_comp_x_breakout16,
             total_fp_x_career,
             games_above_base_todate,
             total_yd_diff_career,
             total_td_career,
             rec_yac_rate_career,
             total_yd_team_x_career,
             rush_fp_share_season,
             rec_fp_share_season) %>%
        gt() %>% 
        data_color(
          columns = vars(
            avg_fp_above_baseline_y1,
            player_age_eos,
            fpob_roll16,
            rec_yptpa_roll16,
            rush_fp_x_share_breakout16,
            rec_comp_x_breakout16,
            total_fp_x_career,
            games_above_base_todate,
            total_yd_diff_career,
            total_td_career,
            rec_yac_rate_career,
            total_yd_team_x_career,
            rush_fp_share_season,
            rec_fp_share_season),
          colors = scales::col_factor(
            palette =  brewer.pal(12,'PRGn')[3:8], #='PRGn',
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
                      "rush_fp_share_season",
                      "rec_fp_share_season",
                      "rec_yac_rate_career"),
          decimals = 1) %>% 
        cols_label(
          comp_name = "Comparison",
          comp_season = "Season",
          ovr_draft_pick = "Pick",
          
          sim_score_adj = "Sim Score",
          avg_fp_above_baseline_y1 = "FPOB Y + 1",
          player_age_eos = "Age",

          fpob_roll16 = "FPOB 16",
          rec_yptpa_roll16 = "YPTPA 16",
          rush_fp_x_share_breakout16 = "EP Share B.O. 16",
          rec_comp_x_breakout16 = "xRec B.O. 16",
          
          total_fp_x_career = "EP/Game",
          games_above_base_todate = "Games Above",
          total_yd_diff_career = "Yards over xYards",
          total_td_career = "TD/Game",
          rec_yac_rate_career = "YAC Rate",
          total_yd_team_x_career = "Team xYard",
          
          rush_fp_share_season = "Rush",
          rec_fp_share_season = "Rec") %>% 
        tab_spanner(
          label = "Career",
          columns = 
            vars(
              total_fp_x_career,
              games_above_base_todate,
              total_yd_diff_career,
              total_td_career,
              total_yd_team_x_career
            )) %>% 
        tab_spanner(
          label = "Season FP Share",
          columns = 
            vars(
              rush_fp_share_season,
              rec_fp_share_season
            )
        )
    } else if (pos %in% c("WR","TE")) {
      df %>% 
        select(comp_name,
               comp_season,
               Pos,
               ovr_draft_pick,
               sim_score_adj,
               avg_fp_above_baseline_y1,
               player_age_eos,
               fpob_roll16,
               rec_comp_diff_roll16,
               rec_adot_roll16,
               rec_yac_rate_roll16,
               rec_racr_roll16,
               total_fp_x_breakout16,
               rec_adot_breakout16,
               rec_yac_rate_breakout16,
               rec_racr_breakout16,
               rec_yptpa_career,
               rec_yac_career,
               total_td_x_career,
               total_fp_diff_career,
               games_above_base_todate,
               pass_fp_team_season,
               rec_ay_share_season) %>%
        gt() %>% 
        data_color(
          columns = vars(
            avg_fp_above_baseline_y1,
            player_age_eos,
            fpob_roll16,
            rec_comp_diff_roll16,
            rec_adot_roll16,
            rec_yac_rate_roll16,
            rec_racr_roll16,
            total_fp_x_breakout16,
            rec_adot_breakout16,
            rec_yac_rate_breakout16,
            rec_racr_breakout16,
            rec_yptpa_career,
            rec_yac_career,
            total_td_x_career,
            total_fp_diff_career,
            games_above_base_todate,
            pass_fp_team_season,
            rec_ay_share_season),
          colors = scales::col_factor(
            palette =  brewer.pal(12,'PRGn')[3:8], #='PRGn',
            domain = NULL)) %>%
        # No Decimals
        fmt_number(
          columns = vars(sim_score_adj),
          decimals = 0) %>%
        #1 Decimal
        fmt_number(
          columns = vars(player_age_eos,
                         fpob_roll16,
                         rec_comp_diff_roll16,
                         rec_adot_roll16,
                         rec_racr_roll16,
                         total_fp_x_breakout16,
                         rec_adot_breakout16,
                         rec_racr_breakout16,
                         rec_yptpa_career,
                         rec_yac_career,
                         total_td_x_career,
                         total_fp_diff_career,
                         pass_fp_team_season,
                         avg_fp_above_baseline_y1),
          decimals = 1) %>%
        #Percent
        fmt_percent(
          columns = vars(rec_yac_rate_roll16,
                         rec_yac_rate_breakout16,
                         rec_ay_share_season),
          decimals = 1) %>% 
        cols_label(
          comp_name = "Comparison",
          comp_season = "Season",
          Pos = "Pos",
          ovr_draft_pick = "Pick",
          
          sim_score_adj = "Sim Score",
          avg_fp_above_baseline_y1 = "FPOB Y + 1",
          player_age_eos = "Age",

          fpob_roll16 = "FPOB",
          rec_comp_diff_roll16 = "Catches over xCatches",
          rec_adot_roll16 = "aDOT",
          rec_yac_rate_roll16 = "YAC Rate",
          rec_racr_roll16 = "RACR",
          
          total_fp_x_breakout16 = "EP",
          rec_adot_breakout16 = "aDOT",
          rec_yac_rate_breakout16 = "YAC Rate",
          rec_racr_breakout16 = "RACR",
          
          rec_yptpa_career = "Rec YPTPA",
          rec_yac_career = "Rec YAC",
          total_td_x_career = "Total xTD",
          total_fp_diff_career = "Total FP over EP",
          games_above_base_todate = "Games Above",
          
          pass_fp_team_season = "Pass FP",
          rec_ay_share_season = "Rec AY%"
          ) %>% 
        tab_spanner(
          label = "Career",
          columns = 
            vars(
              rec_yptpa_career,
              rec_yac_career,
              total_td_x_career,
              total_fp_diff_career,
              games_above_base_todate
            )) %>% 
        tab_spanner(
          label = "Breakout",
          columns = 
            vars(
              total_fp_x_breakout16,
              rec_adot_breakout16,
              rec_yac_rate_breakout16,
              rec_racr_breakout16
            )) %>% 
        tab_spanner(
          label = "Rolling 16",
          columns = 
            vars(
              fpob_roll16,
              rec_comp_diff_roll16,
              rec_adot_roll16,
              rec_yac_rate_roll16,
              rec_racr_roll16
            )
        )
    }
    
  }
  
  output$comp_table <- render_gt({
    req(player_selected(), player_selected_pos()) 
    create_gt(player_selected_pos(), player_selected())
  })

}

# thematic_shiny(font = "auto")
shinyApp(ui, server)
