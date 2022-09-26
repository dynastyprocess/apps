# EP UI Functions
# 
# Because sheesh those UI elements are verbose.

library(bs4Dash)
library(shiny)
# library(echarts4r)
library(dplyr)

ui_header <- function(title,...){
  bs4Dash::dashboardHeader(skin = 'dark',
                           fixed = TRUE,
                           border = TRUE,
                           # compact = TRUE,
                           shiny::span(title,style= 'font-size:1.5em;color:#ffffff'),
                           ...)
}

ui_sidebar <- function(...){
  bs4Dash::dashboardSidebar(title = 'DynastyProcess.com',
                            fixed = TRUE,
                            skin = "dark",
                            elevation = 3,
                            collapsed = TRUE,
                            opacity = 0.8,
                            url = "https://dynastyprocess.com",
                            expand_on_hover = TRUE,
                            sidebar_collapsed = TRUE,
                            src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
                            bs4Dash::sidebarMenu(...))
}

iconwrap_slider <- function(inputid,label,min,max,value,...,icon_left,icon_right){
  shiny::fluidRow(
    bs4Dash::column(2,shiny::icon(icon_left), style = 'padding-top:15px;'),
    bs4Dash::column(8,shiny::sliderInput(inputId = inputid,label = label,min = min, max = max, value = value, ...)),
    bs4Dash::column(2,shiny::icon(icon_right),style = 'padding-top:15px;text-align:right;')
  )
}

# ui_gauge <- function(teamA_total,teamB_total){
#   
#   percent_diff <- dplyr::case_when(teamA_total > teamB_total ~ (teamA_total - teamB_total)/teamB_total,
#                             teamA_total < teamB_total ~ (teamB_total - teamA_total)/teamA_total,
#                             TRUE ~ 0) %>% 
#     round(digits = 2)*100
#   
#   leading_team <- dplyr::case_when(teamA_total == teamB_total ~ "Equal",
#                             percent_diff <= 5 ~ "Fair",
#                             teamA_total > teamB_total ~ "Team A",
#                             teamA_total < teamB_total ~ "Team B")
#   
#   gauge_value <- dplyr::case_when(leading_team == 'Team A' ~ -abs(percent_diff),
#                            TRUE ~ abs(percent_diff))
#   
#   gauge_label <- switch(leading_team,
#                         "Equal" = "Trade is equal!",
#                         "Fair" = "Trade is ~ fair!",
#                         "Team A" = "In Favour of \n Team A",
#                         "Team B" = "In Favour of \n Team B")
#   
#   gauge_colour <- switch(leading_team,
#                         "Equal" = ,
#                         "Fair" = "#333",
#                         "Team A" = "#1b7837",
#                         "Team B" = "#762a83")
#   
#   echarts4r::e_charts() %>%
#     echarts4r::e_gauge(
#       value = gauge_value,
#       name = gauge_label,
#       min = -100,
#       max = 100,
#       splitNumber = 8,
#       splitLine = list(
#         length = 15
#       ),
#       title = list(
#         fontSize = 'large',
#         # fontWeight = 'bold',
#         color = gauge_colour 
#       ),
#       detail = list(
#         fontSize = 'large',
#         formatter = JS("function (value) {return Math.abs(value) + '%';}")
#       ),
#       axisLabel = list(
#         formatter = JS("function (value) {return Math.abs(value) + '%';}")
#       ),
#       # axisTicks = list()
#       axisLine = list(
#         lineStyle = list(
#           width = 15,
#           color = list(
#             c(0.125, '#00441b'),
#             c(0.25, '#1b7837'),
#             c(0.375, '#5aae61'),
#             c(0.5, '#a6dba0'),
#             c(0.625, '#c2a5cf'),
#             c(0.75, '#9970ab'),
#             c(0.875, '#762a83'),
#             c(1, '#40004b'))))
#     )
# }