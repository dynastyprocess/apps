library(bs4Dash)
library(shiny)

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
  tags$li(tags$a(span(icon(icon), style = "font-size:1.1rem;"),
    p(text, style = "margin-left: .5rem;"),
    class = "nav-link", href = href
  ), class = "nav-item")
}

box_about <- function() {
  box(
    width = 12,
    status = "danger",
    title = "About",
    includeMarkdown("about.md")
  )
}

box_leagueselect <- function() {
  box(
    width = 12,
    inputId = "box_leagueselect",
    collapsible = TRUE,
    status = "danger",
    title = "Select League",
    fluidRow(
      column(
        width = 4,
        radioGroupButtons(
          inputId = "platform",
          choices = c("MFL", "Sleeper"),
          selected = "MFL",
          checkIcon = list("yes" = icon("check")),
          status = "danger",
          justified = TRUE
        ),
        uiOutput("league_authbox"),
      ),
      column(
        width = 8,
        id = "column_team_select",
        uiOutput("team_select")
      )
    )
  )
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

#### League Authentication ####

league_auth.mfl <- function() {
  tagList(
    textInput(
      "user_name",
      label = NULL,
      # width = '100%',
      placeholder = "Username"
    ),
    passwordInput(
      "password",
      label = NULL,
      # width = '100%',
      placeholder = "Password"
    ) %>%
      bs4TooltipUI(placement = 'bottom',
        title = "Username / Password are used only to authenticate to the MFL API and is not stored by the app!"),
    actionButton("load_user", "Load My Leagues", class = "btn-success")
  )
}

league_auth.sleeper <- function() {
  tagList(
    textInput(
      "user_name",
      label = NULL,
      # width = '100%',
      placeholder = "Username"
    ),
    actionButton("load_user", "Load My Leagues", class = "btn-success")
  )
}

league_auth.espn <- function() {
  tagList(
    textInput(
      "league_id",
      label = NULL,
      # width = '100%',
      placeholder = "League ID"
    ),
    actionButton("load_league", "Load My Leagues", class = "btn-success")
  )
}

#### Select League ####

user_leagues.ffscrapr <- function(user_obj) {
  conn_user <- ff_connect(
    platform = user_obj$platform,
    season = 2020,
    league_id = user_obj$league_id,
    user_name = user_obj$user_name,
    password = user_obj$password,
    user_agent = "dynastyprocess/apps"
  )

  ff_userleagues(conn_user) %>%
    select(league_id, league_name, franchise_name) %>%
    mutate(
      select = map_chr(
        league_id,
        ~ actionButton(.x,
          "Select",
          class = "btn-primary",
          onclick = 'Shiny.onInputChange(\"league_select\",  this.id)'
        ) %>%
          as.character()
      )
    )
}

team_select.ffscrapr <- function(user_leagues) {
  table <- user_leagues %>%
    select(-contains("_id")) %>%
    reactable(
      defaultColDef = colDef(
        # minWidth = 150,
        header = function(value) make_clean_names(value, "upper_camel", abbreviations = "ID")
      ),
      columns = list(
        # franchise_name = colDef(minWidth = 150),
        select = colDef(
          name = "Select",
          html = TRUE,
          minWidth = 50
        )
      ),
      outlined = TRUE,
      striped = TRUE
    )

  return(div(table))
}

team_select.espn <- function() {
  includeMarkdown("espn_note.md")
}

#### Load Data ####

test_userobj <- function(){list(platform = "MFL",season = 2020, league_id = 54040, user_name = 'solarpool', password = 'M^#63tho')}

load_data.ffscrapr <- function(user_obj, loaded_data) {
  # browser()
  conn <- ff_connect(
    platform = user_obj$platform,
    season = user_obj$season,
    league_id = user_obj$league_id,
    user_name = user_obj$user_name,
    password = user_obj$password,
    user_agent = 'dynastyprocess/apps'
    # rate_limit_number = 2,
    # rate_limit_seconds = 3
  )

  schedule_raw <- ff_schedule(conn) %>%
    select(week, starts_with("franchise"), result, starts_with("opponent")) %>%
    filter(!is.na(franchise_id))

  standings_raw <- ff_standings(conn) %>%
    select(franchise_id, franchise_name, starts_with("h2h"), starts_with("allplay"))

  schedule_unplayed <- schedule_raw %>%
    filter(is.na(franchise_score) | is.na(result)) %>%
    left_join(
      standings_raw %>%
        select(franchise_id, franchise_name, franchise_allplay = allplay_winpct),
      by = "franchise_id"
    ) %>%
    left_join(
      standings_raw %>%
        select(opponent_id = franchise_id, opponent_name = franchise_name, opponent_allplay = allplay_winpct),
      by = "opponent_id"
    ) %>%
    mutate_if(is.numeric,replace_na,0) %>%
    mutate(
      win_probability = franchise_allplay / (franchise_allplay + opponent_allplay),
      win_probability = round(win_probability, 3),
      loss_probability = 1 - win_probability
    ) %>%
    select(-ends_with("score"), -result)

  standings_forecast <- standings_raw %>%
    select(starts_with('franchise'),allplay_winpct,starts_with('h2h')) %>%
    left_join(
      schedule_unplayed %>%
        group_by(franchise_id) %>%
        summarise(
          forecast_wins = sum(win_probability, na.rm = TRUE),
          forecast_losses = sum(loss_probability, na.rm = TRUE)
        ) %>% ungroup(),
      by = 'franchise_id'
    ) %>%
    mutate(total_wins = h2h_wins + forecast_wins,
           total_losses = h2h_losses + forecast_losses,
           total_winpct = total_wins / (total_wins + total_losses + h2h_ties),
           total_winpct = round(total_winpct,3)) %>%
    arrange(desc(total_winpct))

  schedule_pivot <- schedule_unplayed %>%
    left_join(
      standings_forecast %>% select(franchise_id,total_wins),
      by = 'franchise_id'
    ) %>%
    arrange(week,desc(total_wins)) %>%
    select(franchise_name,week,win_probability) %>%
    pivot_wider(names_from = "week",
                values_from = 'win_probability',
                names_prefix = "Week ",
                values_fn = sum) %>%
    clean_names("title")

  loaded_data$schedule_unplayed <- schedule_unplayed
  loaded_data$standings_forecast <- standings_forecast
  loaded_data$schedule_pivot <- schedule_pivot
  loaded_data$schedule_raw <- schedule_raw
  loaded_data$standings_raw <- standings_raw

  return(loaded_data)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

win_colour <- make_color_pal(c("#af8dc3", "#F7F7F7", "#7fbf7b"), bias = 1)
loss_colour <- make_color_pal(c("#7fbf7b", "#F7F7F7", "#af8dc3"), bias = 1)

#### Display Data ####

season_projection <- function(loaded_data){

  table_forecast <- loaded_data$standings_forecast %>%
    select(-franchise_id) %>%
    reactable(
      columns = list(
        franchise_name = colDef(
          minWidth = 250,
          name = "Franchise Name"
        )
      ),
      defaultColDef = colDef(
        header = function(value) {
          make_clean_names(value,'title',abbrev = c("AllPlay","H2H","WinPct"))},
        cell = function(value,index,name) {
          if(str_detect(name,"winpct")){value <- scales::percent(value, accuracy = 0.1)}
          if(is.numeric(value) & !str_detect(name,"h2h|winpct")) {value <- scales::number(value, accuracy = 0.01)}
          return(value)
        },

        style = function(value,index, name){
          x <- list()

          if(is.numeric(value)) x <- c(x,fontFamily = "Ubuntu Mono")

          if(str_detect(name,"win")) {

            normalized <- (value - min(loaded_data$standings_forecast[[name]])) / (max(loaded_data$standings_forecast[[name]]) - min(loaded_data$standings_forecast[[name]]))

            colour <- win_colour(normalized)

            x <- c(x,list(background = colour))}

          if(str_detect(name,"loss")) {

            normalized <- (value - min(loaded_data$standings_forecast[[name]])) / (max(loaded_data$standings_forecast[[name]]) - min(loaded_data$standings_forecast[[name]]))

            colour <- loss_colour(normalized)

            x <- c(x,list(background = colour))
          }

          if(str_detect(name,"h2h_wins|forecast_wins")) { x <- c(x,list(borderLeft = "1px solid #555"))}
          return(x)
        }
      ),
      columnGroups = list(
        colGroup(name = "Current Season", columns = c("h2h_wins","h2h_losses","h2h_ties","h2h_winpct")),
        colGroup(name = "Forecast", columns = c("forecast_wins", "forecast_losses", "total_wins", "total_losses", "total_winpct"))
      ),
      showPageSizeOptions = TRUE,
      defaultPageSize = 25,
      borderless = TRUE,
      showSortIcon = TRUE,
      highlight = TRUE,
      outlined = TRUE,
      striped = TRUE
    )

  box(
    width = 12,
    title = "Season Forecast",
    status = "danger",
    table_forecast
    )

}

weekly_schedule <- function(loaded_data){
  table_weekly <- loaded_data$schedule_pivot %>%
    reactable(
      columns = list(
        `Franchise Name` = colDef(
          minWidth = 250
        )
      ),
      defaultColDef = colDef(
        style = function(value,index, name){
          x <- list()

          if(is.numeric(value)) x <- c(x,fontFamily = "Ubuntu Mono")

          if(str_detect(name,"Week")) {
            normalized <- (value - min(loaded_data$schedule_pivot[[name]])) / (max(loaded_data$schedule_pivot[[name]]) - min(loaded_data$schedule_pivot[[name]]))

            colour <- win_colour(normalized)

            x <- c(x,list(background = colour, fontFamily = "Ubuntu Mono"))
          }

          return(x)
        }
      ),
      showPageSizeOptions = TRUE,
      defaultPageSize = 25,
      borderless = TRUE,
      showSortIcon = TRUE,
      highlight = TRUE,
      outlined = TRUE,
      striped = TRUE
    )

  box(
    width = 12,
    title = "Remaining Schedule",
    status = "danger",
    table_weekly
  )
}
