# EP UI Functions
#
# Because sheesh those UI elements are verbose.

library(bs4Dash)
library(shiny)

.drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

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

external_menuItem <- function(text = NULL, href = NULL, icon = NULL){
  tags$li(tags$a(span(icon(icon),style = "font-size:1.1rem;"),
                 p(text,style = "margin-left: .5rem;"),
                 class = "nav-link", href = href),class = "nav-item")
}

header_box <- function(){

  box(
    width = 12,
    status = "danger",
    title = "About",
    includeMarkdown("about.md")
  )

}

league_select <- function(){
  box(
    width = 12,
    collapsible = TRUE,
    status = "danger",
    title = "Select League",
    fluidRow(
      column(
        width = 6,
        radioGroupButtons(
          inputId = "platform",
          # label = "Platform",
          # direction = "vertical",
          choices = c("MFL","Sleeper","ESPN"),
          selected = "MFL",
          checkIcon = list("yes" = icon("check")),
          status = "danger",
          justified = TRUE),
        uiOutput('league_authbox'),
        ),
      column(
        width = 6,
        uiOutput('team_select')
        )
    )
  )
}

league_authbox.mfl <- function(){
  tagList(
    textInput(
      'user_name',
      label = NULL,
      # width = '100%',
      placeholder = "Username"),
    passwordInput(
      'password',
      label = NULL,
      # width = '100%',
      placeholder = "Password"),
    actionButton("load_user","Load My Leagues",class = "btn-success")
  )
}

league_authbox.sleeper <- function(){

  tagList(
    textInput(
      'user_name',
      label = NULL,
      # width = '100%',
      placeholder = "Username"),
    actionButton("load_user","Load My Leagues",class = "btn-success")
  )

}

league_authbox.espn <- function(){

  tagList(
    textInput(
      'league_id',
      label = NULL,
      # width = '100%',
      placeholder = "League ID"),
    actionButton("load_league","Load My Leagues",class = "btn-success")
  )

}

user_leagues.ffscrapr <- function(user_obj){

  conn_user <- ff_connect(
    platform = user_obj$platform,
    season = 2020,
    league_id = user_obj$league_id,
    user_name = user_obj$user_name,
    password = user_obj$password,
    user_agent = "dynastyprocess/apps"
  )

  ff_userleagues(conn_user) %>%
    select(league_id,league_name,franchise_name) %>%
    mutate(input_id = paste0("leagueid_",league_id),
           select = map_chr(input_id,~actionButton(.x,"Select") %>% as.character))
}

team_select.ffscrapr <- function(user_leagues){
  table <- user_leagues %>%
    select(-contains("_id")) %>%
    reactable(
      defaultColDef = colDef(
        header = function(value) make_clean_names(value,"upper_camel",abbreviations = "ID")),
      columns = list(
        select = colDef(name = "Select",html = TRUE)
      ),
      outlined = TRUE,
      striped = TRUE
    )

  return(div(table))
}

team_select.espn <- function(){
  includeMarkdown('espn_note.md')
}
