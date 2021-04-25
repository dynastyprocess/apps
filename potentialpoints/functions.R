
suppressPackageStartupMessages({
  
  # Data import
  library(ffscrapr)
  library(httr)
  library(jsonlite)
  library(here)
  library(arrow)
  
  # Data manipulation
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(rlang)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(reactable)
  library(DT) 
  library(RColorBrewer)
  library(waiter)
  library(sever)
  
  # Data output
  library(writexl)
  
})

#### UI FUNCTIONS ####

ui_header <- function(title, ...) {
  bs4Dash::dashboardHeader(
    skin = "dark",
    fixed = TRUE,
    border = TRUE,
    # compact = TRUE,
    shiny::span(title, style = "font-size:1.5em;color:#ffffff"),
    ...
  )
}

ui_sidebar <- function(...) {
  bs4Dash::dashboardSidebar(
    title = "Apps",
    fixed = TRUE,
    skin = "dark",
    elevation = 3,
    collapsed = TRUE,
    opacity = 0.8,
    url = "https://dynastyprocess.com",
    expand_on_hover = TRUE,
    src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
    bs4Dash::sidebarMenu(...)
  )
}

external_menuItem <- function(text = NULL, href = NULL, icon = NULL) {
  tags$li(
    tags$a(span(icon(icon), style = "font-size:1.1rem;"),
           p(text, style = "margin-left: .5rem;"),
           class = "nav-link", href = href
    ), class = "nav-item")
}


sever_dp <- function(){
  
  sever::sever(
    shiny::tagList(
      shiny::h1("Disconnected"),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
}

#### UI BOXES ####

box_about <- function() {
  box(
    width = 4,
    inputId = 'box_about',
    status = "danger",
    title = "About",
    includeMarkdown("about.md"),
    footer = actionButton('debug_please',"Is My League A Public League?")
  )
}

box_inputs <- function(){
  box(
    width = 8,
    inputId = 'box_inputs',
    status = "danger",
    title = "Select League",
    fluidRow(
      
      column(
        width = 4,
        textInput(
          'league_id', label = 'ESPN League ID', value = '1178049',
          width = '100%', placeholder = "(Public Leagues Only)")),
      column(
        width = 4,
        sliderInput(
          'week_select', label = "Select Weeks", value = c(1,17),
          min = 1, max = 17, width = '100%', ticks = FALSE)
      ),
      column(
        width = 4,
        selectInput(
          'year_select', label = "Select Year", selected = .choose_season(),
          choices = c(.choose_season():2019),multiple = FALSE, width = '100%')
      )
    ),
    footer = 
      span(
        actionButton('load','Calculate!',icon = icon('calculator')),
        uiOutput('download_button', inline = TRUE)
      )
  )
}

.choose_season <- function(date = NULL){
  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (class(date) != "Date") {
    date <- as.Date(date)
  }
  if (as.numeric(format(date, "%m")) > 8) {
    return(format(date, "%Y"))
  }
  return(format(date - 365.25, "%Y"))
}

box_summary <- function(){
    tabBox(width = 12, title = 'Summary',side = 'right',id = 'summary_data',
         status = 'danger',tabStatus = 'dark',
         tabPanel('Total', DTOutput('summary_season')),
         tabPanel('Weekly', DTOutput('summary_week')),
         tabPanel('Details', DTOutput('details')))
  
}

#### SERVER ####

get_leagueinfo <- function(league_id,year,week_select){
  espn <- fromJSON(
    paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/',
           year,
           '/segments/0/leagues/',
           league_id,
           '?view=mSettings',
           '&view=mTeam'),
    flatten=TRUE)
  
  league_name <- espn %>% pluck('settings','name')
  
  current_week <- espn %>% pluck('status','currentMatchupPeriod')
  
  max_week <- if_else(week_select[2] <=current_week,week_select[2],current_week)
  
  teams <- espn %>% 
    pluck('teams') %>% 
    mutate(team_name = paste(location,nickname)) %>% 
    select(team_id = id,primary_user = primaryOwner,team_name)
  
  users <- espn %>% 
    pluck('members') %>% 
    select(id,user_name = displayName) %>% 
    left_join(teams, by = c('id' = 'primary_user')) %>% 
    unnest(c(team_id,team_name))
  
  return(
    list(
      league_name = league_name,
      league_id = league_id,
      year = year,
      current_week = current_week,
      max_week = max_week,
      users = users
    )
  )
}

get_potentialpoints <-function(league_id, year, scoreweek){
  
  
  espn <- fromJSON(
    paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/',
           year,
           '/segments/0/leagues/',
           league_id,
           '?scoringPeriodId=',
           scoreweek,
           '&view=mMatchupScore',
           '&view=mBoxscore',
           #'&view=mScoreboard',
           #'&view=mTeam', 
           #'&view=mRoster',
           '&view=mSettings',
           '&view=mRosterSettings'
           #'&view=kona_player_info',
           #'&view=mNav'
    ),flatten = TRUE)
  
  lineup_keys <-tibble(lineup_id=c(0,2,3,4,5,6,7,16,17,20,21,23,
                                      8,9,10,11,24,12,13,14,15),
                          pos=c('QB','RB','RB/WR','WR','WR/TE','TE',
                                'OP','DST','K','BE','IR','FLEX',
                                'DT','DE','LB','DL','EDR','CB','S','DB','DP'),
                          priority=c(1,2,5,3,6,4,8,9,10,0,0,11,
                                     12,13,16,14,15,17,18,19,20)) %>% 
    arrange(lineup_id) %>% 
    mutate(lineup_id=as.character(lineup_id))
  
  lineup_settings <- espn %>% 
    pluck('settings','rosterSettings','lineupSlotCounts') %>% 
    enframe(name = 'lineup_id',
            value = 'count') %>%
    left_join(lineup_keys, by = 'lineup_id') %>% 
    filter(count!= 0 & priority!=0) %>% 
    arrange(priority)

  schedule<-espn$schedule %>% 
    select(week=matchupPeriodId,away.teamId,away.entries=away.rosterForCurrentScoringPeriod.entries,
           home.teamId,home.entries=home.rosterForCurrentScoringPeriod.entries, 
           home.points=home.totalPoints,away.points=away.totalPoints) %>% 
    filter(away.entries!='NULL')
  
  playerweeks <- schedule %>%
    select(
      week,
      home.teamId = away.teamId,
      home.entries = away.entries,
      home.points = away.points
    ) %>%
    bind_rows(schedule) %>%
    select(week,
           team_id = home.teamId,
           score = home.points,
           entries = home.entries) %>%
    hoist(
      entries,
      actual_lineup = 'lineupSlotId',
      player_id = 'playerId',
      points = 'playerPoolEntry.appliedStatTotal',
      player = 'playerPoolEntry.player.fullName',
      eligible = 'playerPoolEntry.player.eligibleSlots'
    ) %>%
    unnest(c(actual_lineup, player_id, points, player, eligible)) %>%
    unnest_longer(eligible) %>% 
    mutate(eligible=as.character(eligible)) %>% 
    select(-entries)
  
  unusedplayers<-playerweeks
  starters<-tibble()
  
  optimal_lineups<-tibble()
  
  for (i in lineup_settings$priority) {
    pos<-lineup_settings %>% 
      filter(priority==i) %>% 
      nest_join(unusedplayers,by=c('lineup_id'='eligible')) %>% 
      unnest_wider(unusedplayers) %>% 
      unnest(-(1:3)) %>% 
      group_by(team_id) %>% 
      mutate(rank=rank(desc(points),ties.method=c('first'))) %>% 
      filter(rank<=count)
    
    starters<-bind_rows(starters,pos) %>% 
      arrange(team_id,priority,desc(points))
    
    unusedplayers<-unusedplayers %>% 
      anti_join(pos,by=c('player_id'))
  }
  
  optimal_lineups<-bind_rows(optimal_lineups,starters)
  
  return(optimal_lineups)
}
