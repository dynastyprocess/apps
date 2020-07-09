####  UI functions ####
suppressPackageStartupMessages({
library(bs4Dash)
library(shiny)
library(sever) # github::johncoene/sever
library(dplyr)
})

ui_header <- function(...){
  bs4Dash::dashboardHeader(
    skin = 'dark',
    fixed = TRUE,
    status = 'danger',
    border = TRUE,
    shiny::span(shiny::icon('user-friends'),'Community Rookie ADP',style= 'font-size:1.5em;color:#ffffff'),
    ...)
}

ui_sidebar <- function(...){
  bs4Dash::dashboardSidebar(
    title = 'DynastyProcess.com',
    fixed = TRUE,
    brandColor = 'danger',
    status = 'danger',
    elevation = 3,
    opacity = 0.8,
    url = "https://dynastyprocess.com",
    expand_on_hover = TRUE,
    src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
    skin = 'light',
    bs4Dash::sidebarMenu(...))
}

ui_body <- function(...){
  bs4Dash::dashboardBody(
    sever::use_sever(),
    bs4Dash::tabItems(
      ...
    )
  )
}


