library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinyjs)
library(DT)
library(tidyr)
library(shinyWidgets)
library(grid)

x <- read_csv("https://raw.githubusercontent.com/DynastyProcess/data/master/files/db_fpecr.csv") %>%
  filter(ecr_type %in% c('dp','rp'), pos %in% c('QB','RB','WR','TE')) %>%
  select(player_name, fantasypros_id, pos, team, scrape_date, ecr, ecr_type) %>%
  pivot_wider(names_from = ecr_type,
              values_from = ecr,
              values_fn = min) %>%
  ungroup() %>%
  transmute(name = as.factor(player_name), pos = as.factor(pos), tm = as.factor(team), date = as.factor(scrape_date), dynpECR = dp, rdpECR = rp) %>%
  arrange(date, dynpECR) %>%
  as.data.frame()

dates <- tail(unique(x$date),3)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  withTags(
    nav(class="navbar navbar-default navbar-static-top", role="navigation",
        div(class="container-fluid",
            div(class="navbar-header",
                span(class="navbar-brand",
                     a(href="https://dynastyprocess.com",strong("DynastyProcess.com"))
                ),
                button(type="button", class="navbar-toggle", `data-toggle`="collapse", `data-target`="#myNavbar",
                       span(class="icon-bar"),
                       span(class="icon-bar"),
                       span(class="icon-bar")
                       )
            ),
            div(class="collapse navbar-collapse", id="myNavbar",
            ul(class="nav navbar-nav",
               li(a(href="http://apps.dynastyprocess.com/database",strong("Database"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/calculator",strong("Calculator"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/rookie-adp",strong("Rookie ADP"))
               ),
               li(class="dropdown",
                  a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                  ul(class="dropdown-menu",
                     li(a(href="http://apps.dynastyprocess.com/arbitrage",strong("Arbitrage"))),
                     li(class="active",a(href="#",strong("ECR Explorer"))),
                     li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                  )
               )
            ))
        )
    )
  ),
  titlePanel("DynastyProcess.com ECR Explorer"),
  shinyjs::useShinyjs(),
  id = "options",
  br(),
  fluidRow(
    column(1,h4("About"),style='text-align:center'),
    column(10,p("Use the ECR Explorer to examine trends in FantasyPros redraft and dynasty positional ranks, and to identify win-now or rebuild targets. To zoom in on a specific area, click & drag to select the area, then double-click to focus. Double-click to reset."))),
  br(),
  fluidRow(column(3,
                  radioGroupButtons("posFilter", "Position:",
                                    choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
                                    selected = "QB",
                                    status = "success",
                                    justified = TRUE)),
           column(3,
                  sliderInput("playerRange",
                              "Filter ECR Range:",
                              min = 1,
                              max = 64,
                              width = '100%',
                              value = c(1,24)
                  )),
           column(3,
                  sliderTextInput("DateRange",
                                  "Select Date Range:",
                                  width = '100%',
                                  choices = unique(x$date),
                                  selected = unique(x$date)[c(length(unique(x$date)) - 7,length(unique(x$date)))]
                  )
           ),
           column(3,
                  selectizeInput("playerList",
                                 "Select Players",
                                 choices = c(x["name"]),
                                 selected = NULL,
                                 width = '100%',
                                 multiple = TRUE),
                  actionButton("clear1", "Clear Selected Players", icon = icon("trash"), class="btn radiobtn btn-success"))
  ),
  hr(),
  # hr(),
  #mainPanel(
  tags$head(
    tags$style(type='text/css', 
               ".nav-tabs {font-size: large; font-weight: bold} ")),
  tags$head(
    tags$style(type='text/css', 
               ".form-control {height: 30px; padding: 0px 15px} ")),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", icon=icon("chart-line"),
                           fluidRow(column(6,
                                           #offset=1,
                                           plotOutput("distPlot",
                                                      height = "800px",
                                                      # width = "100%",
                                                      dblclick = "dblclick",
                                                      brush = brushOpts(
                                                        id = "plot1_brush",
                                                        resetOnNew = TRUE),
                                                      
                                                      #width = "100%",
                                                      hover = hoverOpts(id= "plot_hover",
                                                                        delay = "100",
                                                                        delayType = "throttle"),
                                                      click = "plot_click"),
                                           uiOutput("hover_info"),
                                           uiOutput("hover_info2")),
                                    column(6,#offset=1,
                                           div(DTOutput("printData")), style='font-size: small',
                                           #DTOutput("printData")),
                                           
                                           br(),
                                           downloadButton("downloadData1", "Download", class="btn radiobtn btn-success"))
                           ))
                  ,
                  # tabPanel("Plot Data",
                  #          br(),
                  #          #downloadButton("downloadData1", "Download"),
                  #          #br(),
                  #          #br(),
                  #          #fluidRow(
                  #            column(9, DTOutput("printData")),
                  #            column(2, downloadButton("downloadData1", "Download"),offset=1)
                  #          #)
                  # ),
                  tabPanel("All Data", icon=icon("table"),
                           br(),
                           #downloadButton("downloadData2", "Download"),
                           #br(),
                           #br(),
                           #fluidRow(
                           div(column(9, DTOutput("printData2")),style="font-size: small"),
                           column(2, downloadButton("downloadData2", "Download",class="btn radiobtn btn-success"),offset=1)
                           #)
                  )
  ) 
)
)
#)
#)
