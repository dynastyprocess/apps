suppressPackageStartupMessages({
  # Data import
  library(here)
  # Data manipulation
  library(tidyverse)
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(gt)
  library(DT)
})

source('fn_ui_desktop.R')

setwd(here())
salaries <- read_csv("auctioneer_salaries.csv") %>%
  filter(!is.na(ppg_rank))

# library(slider)
# temp <- salaries %>% filter(ppg_year == 2019, ppg_rank/salary > 1)
# temp <- salaries  %>% 
#   filter(ppg_year == 2019) %>%
#   arrange(position, ppg_rank) %>%
#   group_by(position) %>%
#   mutate(qual = slide_dbl(salary, mean, .before = 3, .after = 3, .complete = TRUE)) %>%
#   ungroup() %>%
#   mutate(diff = salary - qual)

get_ppg_rank <- function(input_name, input_year) {
  rank <- salaries %>%
    filter(ppg_year == input_year,
           full_name == input_name) %>%
    select(ppg_rank) %>%
    pull()
  
  if(length(rank) == 0 | is.null(rank)) {rank <- 9999}
  else if(is.na(rank)) {rank <- 9999}
  
  rank
}

get_position <- function(input_name, input_year) {
  salaries %>%
    filter(ppg_year == input_year,
           full_name == input_name) %>%
    select(position) %>%
    pull()
}

get_games <- function(input_name, input_year) {
  games <- salaries %>%
    filter(ppg_year == input_year,
           full_name == input_name) %>%
    select(games) %>%
    pull()
  
  if (length(games) == 0) {games <- 0}
  
  games
}

get_draft_year <- function(input_name) {
  salaries %>%
    filter(full_name == input_name) %>%
    summarise(max(draft_year)) %>%
    pull()
}

get_position_max <- function(input_pos, input_year) {
  salaries %>%
    filter(position == input_pos,
           ppg_year == input_year) %>%
    summarise(max(salary))
}

get_position_count <- function(input_pos, input_year) {
  salaries %>%
    filter(position == input_pos,
           ppg_year == input_year) %>%
    tally() %>%
    pull()
}

get_comparables <- function(input_name, input_year){
  rank <- get_ppg_rank(input_name,input_year)
  pos <- get_position(input_name,input_year)
  pos_tally <- ifelse(is_empty(pos), 0, get_position_count(pos, input_year))

  if (get_draft_year(input_name) == 2020 | length(rank) == 0 | rank == 9999) {
    return(tibble())
  }
  
  rank_min <- case_when(rank <= 3 ~ 1L,
                        pos_tally - rank <= 3 ~ as.integer(pos_tally - 6),
                        TRUE ~ as.integer(rank - 3))
  
  rank_max <- case_when(rank <= 3 ~ 6L,
                        pos_tally - rank <= 3 ~ as.integer(pos_tally),
                        TRUE ~ as.integer(rank + 3))
  
  # rank_min <- ifelse(rank <= 3, 1, rank - 3)
  # rank_max <- ifelse(rank <= 3, 6, rank + 3)
  
  comps <- salaries %>% 
    filter(ppg_year == input_year,
           position == pos,
           between(ppg_rank, rank_min, rank_max))
  
  min_drop <- ifelse(rank <= 3, 2, 1)
  
  minName<- comps %>%
    filter(full_name != input_name) %>%
    arrange(-ppg_rank) %>%
    slice_min(salary, n = min_drop, with_ties = FALSE) %>%
    select(full_name) %>%
    pull()
  
  maxName <- comps %>%
    filter(full_name != input_name) %>%
    arrange(ppg_rank) %>%
    slice_max(salary, n = 1, with_ties = FALSE) %>%
    select(full_name) %>%
    pull()
  
  if (rank <= 3) {maxName <- ''}
  
  
  comps %>%
    mutate(colorFlag = case_when(full_name == input_name ~ 'purple',
                                 full_name %in% minName | full_name %in% maxName ~ 'red',
                                 TRUE ~ 'green'),
           meanWeight = ifelse(input_year == 2018,1,3))
}

create_gt <- function(df, input_player) {
  
  df %>%
    gt() %>%
    tab_style(
      style = cell_fill(color = "lightgreen"),
      locations = cells_body(
        rows = colorFlag == "green")) %>%
    tab_style(
      style = cell_fill(color = "#EFD8CA"),
      locations = cells_body(
        rows = colorFlag == "red")) %>%
    tab_style(
      style = cell_fill(color = "#DFCAEF"),
      locations = cells_body(
        rows = colorFlag == "purple")) %>%
    # cols_label(
    #   full_name = "Player Name",
    #   ppg_rank = "PPG Rank",
    #   ppg = "PPG"
    # ) %>%
    cols_hide(columns = vars(draft_year, ppg_year, colorFlag)) %>%
    tab_options(table.width = pct(100))
  
}

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  navbar = ui_header(),
  sidebar = ui_sidebar(
    menuItem('QO Explorer',tabName = 'qual',icon = 'table')),
  dashboardBody(tabItems(
    tabItem(tabName = 'qual',
            titlePanel('Qualifying Offer Explorer'),
            fluidRow(
              box(#title = "Inputs",
                status = "danger",
                width = 4,
                
                column(width = 12,
                       selectizeInput("selectPlayer",
                                      label = "Select a Player:",
                                      choices = unique(salaries$full_name),
                                      selected = "Josh Allen"))),
              box(status = "danger",
                  width = 8,
                  htmlOutput("salaryText"))
            ),
            fluidRow(
              box(title = "2018 Comparisons",
                  status = "danger",
                  width = 6,
                  gt_output("table18"),
                  htmlOutput("year1Text")),
              box(title = "2019 Comparisons",
                  status = "danger",
                  width = 6,
                  gt_output("table19"),
                  htmlOutput("year2Text"))
            )
    )
  )
  )
)

server <- function(input, output, session) {
  
  players18 <- reactive({
    get_comparables(input$selectPlayer, 2018)
  })
  
  players19 <- reactive({
    get_comparables(input$selectPlayer, 2019)
  })
  
  output$table18 <- render_gt({
    req(input$selectPlayer)
    req(length(players18()) > 0)
  
    
    create_gt(players18(), input$selectPlayer)
  })
  
  output$table19 <- render_gt({
    req(input$selectPlayer)
    req(length(players19()) > 0)
    
    create_gt(players19(), input$selectPlayer)
  })
  
  output$year1Text <- renderText({
    req(input$selectPlayer)
    
    if (get_games(input$selectPlayer,2018) < 6) {
      paste0("<span style=\"font-size: 28px;\">",input$selectPlayer ," had fewer than 6 games in 2018.</span>")
    } else {
      salaryNumMedian<- players18() %>%
        filter(colorFlag == "green") %>%
        summarise(mean(salary)) %>%
        round(1)
      
      paste0("<span style=\"font-size: 20px;\">The mean QO for <strong><span style=\"color: rgb(226, 80, 65);\">", input$selectPlayer, "</span></strong> would've been <strong><span style=\"color: rgb(226, 80, 65);\">", salaryNumMedian, "</span></strong> using only 2018.</span>")
    }
    
  })
  
  output$year2Text <- renderText({
    req(input$selectPlayer)
    
    if (get_games(input$selectPlayer,2019) < 6) {
       paste0("<span style=\"font-size: 28px;\">",input$selectPlayer ," had fewer than 6 games in 2019.</span>")
    }  else {
      salaryNumMedian<- players19() %>%
        filter(colorFlag == "green") %>%
        summarise(mean(salary)) %>%
        round(1)
      
      paste0("<span style=\"font-size: 20px;\">The mean QO for <strong><span style=\"color: rgb(226, 80, 65);\">", input$selectPlayer, "</span></strong> would've been <strong><span style=\"color: rgb(226, 80, 65);\">", salaryNumMedian, "</span></strong> using only 2019.</span>")
    }

    
  })
  
  output$salaryText <- renderText({
    req(input$selectPlayer)
    
    if (get_draft_year(input$selectPlayer) == 2020) {
      return("<span style=\"font-size: 28px;\">You picked a 2020 rookie! They don't have a PPG history to compare yet.</span>")
    } else if (get_games(input$selectPlayer,2018) < 6 & get_games(input$selectPlayer,2019) < 6) {
      return("<span style=\"font-size: 28px;\">This player has fewer than 6 games in both seasons.</span>")
    }
    
    salaryNum <- players18() %>%
      bind_rows(players19()) %>%
      filter(colorFlag == "green") %>%
      summarise(mean(salary)) %>%
      round(1)
    
    # salaryNumWeight <- players18() %>%
    #   bind_rows(players19()) %>%
    #   filter(colorFlag == "green") %>%
    #   summarise(weighted.mean(salary, meanWeight)) %>%
    #   round(1)
    # 
    # salaryNumOutliers <- players18() %>%
    #   bind_rows(players19()) %>%
    #   filter(colorFlag != "purple") %>%
    #   summarise(mean(salary)) %>%
    #   round(1)
    # 
    # salaryNumAll<- players18() %>%
    #   bind_rows(players19()) %>%
    #   summarise(mean(salary)) %>%
    #   round(1)
    # 
    # salaryNumMedian<- players18() %>%
    #   bind_rows(players19()) %>%
    #   summarise(median(salary)) %>%
    #   round(1)
    
    if (get_ppg_rank(input$selectPlayer, 2018) <= 3 & get_ppg_rank(input$selectPlayer, 2019) <= 3){
      salaryNum <- get_position_max(get_position(input$selectPlayer, 2019), 2019)
    }
    
    str <- paste0("<span style=\"font-size: 28px;\">The Qualifying Offer for <strong><span style=\"color: rgb(226, 80, 65);\">", input$selectPlayer, "</span></strong> would've been <strong><span style=\"color: rgb(226, 80, 65);\">", salaryNum, "</span></strong> this offseason.</span>")
                  
    
    if (get_ppg_rank(input$selectPlayer, 2018) <= 3 & get_ppg_rank(input$selectPlayer, 2019) <= 3){
      str <- paste0(str, "<span style=\"font-size: 28px;\"><br> He finished in the top 3 both seasons.</span>")
    } 
    
    str
    
  })
  
}

shinyApp(ui, server)