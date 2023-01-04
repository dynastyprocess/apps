
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
    title = "DP Apps",
    fixed = TRUE,
    skin = "dark",
    elevation = 3,
    collapsed = TRUE,
    opacity = 0.8,
    url = "https://dynastyprocess.com",
    expand_on_hover = TRUE,
    src = "https://raw.githubusercontent.com/dynastyprocess/graphics/main/.dynastyprocess/logohexonly.png",
    bs4Dash::sidebarMenu(...)
  )
}