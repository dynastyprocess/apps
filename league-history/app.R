# SETUP  ----------------------------------------------------------
options(ffscrapr.cache = "filesystem")
suppressPackageStartupMessages({
  library(ffscrapr)
  library(ffpros)
  library(nflreadr)
  library(tidyverse)
  library(gt)
  library(gtExtras)
  library(RColorBrewer)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(gfonts)
  library(waiter)
  
})

options(warn=1, dplyr.summarise.inform = FALSE)

# UI section --------------------------------------------------------------
ui <- dashboardPage(
  title = "League History - DynastyProcess.com",
  header = ui_header("League History App"),
  sidebar = ui_sidebar(
    menuItem('History', tabName = 'history', icon = shiny::icon('table')),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "cloud")
  ),
  dashboardBody(
    use_font("fira-sans-condensed", "www/css/fira-sans-condensed.css"),
    use_waiter(),
    tabItems(
      tabItem(tabName = 'history',
              h1('League History', style = "padding-left:10px;"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           selectizeInput("select_platform",
                                          "Select Platform:",
                                          choices = c("Sleeper" = "sleeper"),
                                          multiple = FALSE,
                                          selected = "sleeper")
                    ),
                    column(width = 4,
                           textInput("league_id", label = "Enter League ID",
                                     value = "786958282513362944")),
                    column(width = 4,
                           actionButton("load_data",
                                        "Load League",
                                        class = "btn-success")))))),
    box(width = 12,
        fluidRow(width = 12,
                 gt_output("league_table")))
  )
)


# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  input_connection <- reactiveVal()
  league_df <- reactiveVal()
  
  #react on enter button
  observeEvent({input$load_data},{
    
    waiter_show(
      html = spin_dots(),
      color = transparent(0.5))
    
    input_connection <- reactive({
      ffscrapr::ff_connect(platform = input$select_platform,
                           league_id = input$league_id,
                           season = 2022,
                           rate_limit_number = 1000,
                           rate_limit_seconds = 60)
    })
    
    league_df <- reactive({
      get_league_history(platform = input$select_platform,
                         league_id = input$league_id,
                         conn = input_connection())
    })
    
    league_info <- reactive({
      ff_league(input_connection())
    })
    
    year_range <- reactive({
      league_info() %>%
        separate(col = years_active,
                 into = c("min_year", "max_year"),
                 sep = "-")
    })
    
    output$league_table <- render_gt({

      league_df() %>% 
        gt() %>%
        tab_header(title = glue::glue("{league_info()$league_name} {league_info()$years_active}")) %>%
        cols_label(user_name = "Team",
                   emoji_collase = glue::glue("{year_range()$min_year}\U2192{year_range()$max_year}"),
                   allplay_winpct = "AP Win %",
                   h2h_winpct = "Win %",
                   luck_pct = "Luck %",
                   regular_season_record = "Record",
                   optimal_start_percent = "PP %",
                   
                   playoff_record = "Record",
                   playoff_h2h_winpct = "Win %",
                   
                   first_round_picks = "Firsts",
                   total_picks = "Total",
                   value_by_season = "Value",
                   trade_count = "Trades",
                   players_added = "Adds",
                   players_added_by_season = "History") %>% 
        fmt_percent(columns = c(allplay_winpct, h2h_winpct, luck_pct, optimal_start_percent, playoff_h2h_winpct),
                    decimals = 1) %>%
        tab_spanner(label = "Regular Season",
                    columns = c(allplay_winpct, optimal_start_percent, regular_season_record, h2h_winpct, luck_pct)) %>%
        tab_spanner(label = "Playoffs",
                    columns = c(playoff_record, playoff_h2h_winpct)) %>%
        tab_spanner(label = "Picks",
                    columns = c(first_round_picks, total_picks, value_by_season)) %>% 
        tab_spanner(label = "Transactions",
                    columns = c(trade_count, players_added, players_added_by_season)) %>% 
        
        cols_align(align = "right",
                   columns = c(emoji_collase)) %>%
        tab_style(style = cell_text(whitespace = "nowrap"),
                  locations = cells_body(columns = emoji_collase)) %>% 
        gt_plt_sparkline(value_by_season,
                         type = "shaded") %>% 
        gt_plt_sparkline(players_added_by_season,
                         type = "shaded") %>% 
        data_color(
          columns = c(optimal_start_percent,
                      h2h_winpct,
                      allplay_winpct,
                      luck_pct,
                      playoff_h2h_winpct,
                      first_round_picks,
                      total_picks,
                      trade_count,
                      players_added),
          colors = scales::col_factor(
            brewer.pal(11,'PRGn')[3:8],
            domain = NULL
          )) %>%
        cols_move(columns = c(emoji_collase,
                              allplay_winpct,
                              h2h_winpct,
                              luck_pct,
                              regular_season_record,
                              optimal_start_percent,
                              playoff_record,
                              playoff_h2h_winpct,
                              first_round_picks,
                              total_picks,
                              value_by_season,
                              trade_count,
                              players_added,
                              players_added_by_season),
                  after = user_name) %>% 
        tab_footnote(footnote = "What would your win rate be if you played every team each week?",
                     locations = cells_column_labels(columns = allplay_winpct)) %>% 
        tab_footnote(footnote = "Difference between your win rate and all play win rate",
                     locations = cells_column_labels(columns = luck_pct)) %>%
        tab_footnote(footnote = "Percentage of potential points",
                     locations = cells_column_labels(columns = optimal_start_percent)) %>% 
        tab_footnote(footnote = "Playoff byes counted as 1 win",
                     locations = cells_column_labels(columns = playoff_record)) %>% 
        tab_source_note(source_note = emoji_glue(":trophy: Best Team, :2nd_place_medal: 2nd Place, :3rd_place_medal: 3rd Place, :check_mark: Playoffs, :x: Missed Playoffs, :taco: Worst Team, :black_square_button: Didn't Play")) %>% 
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_column_spanners(everything())
        ) %>% 
        opt_table_lines(extent = "default") %>%
        tab_options(
          column_labels.border.top.color = "black",
          column_labels.border.top.width = px(3),
          column_labels.border.bottom.color = "black",
          table_body.hlines.color = "white",
          table_body.hlines.width = px(0),
          table.border.bottom.color = "white",
          table.border.bottom.width = px(3),
          table_body.border.bottom.color = "black",
          table.border.top.color = "black",
          table.border.top.width = px(3)
        )
    })
    
  })
  
  
}

shinyApp(ui, server)