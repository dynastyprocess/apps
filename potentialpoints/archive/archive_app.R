library(jsonlite)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(markdown)

ui <-
  dashboardPage(skin = "blue", title = "DynastyProcess Apps: Potential Points Calculator",
    dashboardHeader(title = a(href = "https://dynastyprocess.com", img(src = "logo-horizontal.png", width ='100%')), titleWidth = 250),
    dashboardSidebar(
      #sidebarmenus----
      width = 250,
      sidebarMenu(menuItem("ESPN Potential Points", tabName = "espnpp", icon = icon("tv"))
      ),
      sidebarMenu(
        menuItem(
          "More from DynastyProcess:",
          icon = icon("rocket"),
          menuSubItem("Calculator", icon =
                        icon('calculator'), href = "https://apps.dynastyprocess.com/calculator"),
          menuSubItem("Database", icon =
                        icon('database'), href = "https://apps.dynastyprocess.com/database"),
          menuSubItem("Crystal Ball", icon =
                        icon('quidditch'), href = "https://apps.dynastyprocess.com/crystalball"),
          menuSubItem("More!", icon =
                        icon('rocket'), href = "https://dynastyprocess.com/apps")
        ))
    ),
    dashboardBody(tags$head(
      #CSS----
      tags$link(rel = "stylesheet", type = "text/css", href = "www/flatly.css"),
      tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #000;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #555;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                  background-color: #000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                  background-color: #000;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #555;
                                  text-decoration: none;
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #555;
                                  text-decoration: none;
                                }
                                .skin-blue .sidebar-menu > li.active > a{
                                  border-left-color: #fff
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #fff;
                                }
                                .btn {
                                  font-size: 12px;
                                }

                                .selectize-input
                                {font-size:12px;
                                min-height:25px;
                                padding-top:0px;
                                padding-bottom:0px;
                                }

        '))
    ),
    #Content----
    tabItems(
      tabItem(tabName='espnpp',
              fluidRow(
                box(width=12, 
                    titlePanel("DynastyProcess: Potential Points Calculator"),
                    includeMarkdown('about.md')
                    )
              ),
              fluidRow(
                box(width=8,
                    textInput('leagueid',label='ESPN League ID',value='1178049',placeholder="(Public Leagues Only!)"),
                    sliderInput('weekselect',label = "Select Weeks", value=c(1,17),min=1,max=17, ticks=FALSE),
                    actionButton('load','Calculate!',icon=icon('calculator')),
                    br(),
                    textOutput('leaguename')
                    ),
                box(width=4,title='Downloads',
                    downloadButton('downloadseason',label="Download PP Summary"),
                    downloadButton('downloadweek',label='Download Weekly Breakdown'))
              ),
              fluidRow(
                    tabBox(width = 12,title = "Summary",side='right',
                    tabPanel('Total',DTOutput('summary_season')),
                    tabPanel('Weekly Breakdown',DTOutput('summary_week'))
              )
),

              fluidRow(
                box(title='Details',width=12,solidHeader = TRUE,
                    DTOutput('details'))
              ),

              uiOutput('debugbox')
              )
      )
      
    )
  )
  

server <- function(input, output, session) {
  
  espnbasic<- eventReactive(input$load,fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/',
                              input$leagueid,
                              '?view=mSettings',
                              '&view=mTeam'),flatten=TRUE))
  
  leaguename<-reactive(espnbasic()$settings$name)
  
  currentweek<-reactive(espnbasic()$status$currentMatchupPeriod)
  
  maxweek<-reactive(if_else(input$weekselect[2]<=currentweek(),input$weekselect[2],currentweek()))
  
  output$leaguename<-renderText(paste('Loaded:',leaguename()))
  
  teams<-reactive({espnbasic()$teams %>% 
    select(id,primaryOwner, location, nickname) %>% 
    mutate(team_name=paste(location,nickname)) %>% 
    select(id,primaryOwner,team_name)})
  
  owners <- reactive({
    espnbasic()$members %>%
      select(id, owner_name = displayName) %>%
      left_join(
        teams() %>% select('team_id' = 'id','team_name','primaryOwner'),
        by = c('id' = 'primaryOwner')) %>%
      unnest(team_id, team_name)
  })

  
  details<-eventReactive(input$load,{
   tibble(league_id=input$leagueid,weeklist=c(input$weekselect[1]:maxweek())) %>%
      rowwise() %>% 
      mutate(lineups=lapply(league_id,ppfunction,weeklist)) %>% 
      unnest(lineups) %>% 
      left_join(owners(),by="team_id") %>% 
      select(Week=week,Owner=owner_name,Team=team_name,Pos=pos,ActualScore=score,Player=player,Points=points)
  })
  
  summary_week<-eventReactive(input$load,{
    details() %>% 
      group_by(Owner,Week,Team) %>%
      summarize(ActualScore=mean(ActualScore),PotentialScore=sum(Points)) %>% 
      ungroup()
  })
  
  summary_season<-eventReactive(input$load,{
    summary_week() %>% 
      group_by(Owner,Team) %>% 
      summarize(ActualScore=sum(ActualScore),PotentialScore=sum(PotentialScore)) %>% 
      arrange(desc(PotentialScore))
  })
  
  errortext<-eventReactive(input$load,{
    req(input$leagueid)
    paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/',as.character(input$leagueid),'?view=mSettings')
    # HTML(a(href=paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',as.character(input$leagueid),'&view=mSettings'),'Link'))
  })
  
  output$debugbox<-renderUI(fluidRow(box(title='Debug Link',width=12,
                                         HTML(renderMarkdown(text=paste0("[Having trouble? Open this page in an incognito window to see if the app is reading the ESPN API correctly!](",errortext(),")")))
                                         )))
  
  output$details<-renderDT(details(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=25))
  
  output$summary_week<-renderDT(summary_week(),rownames=FALSE,options=list(scrollX=TRUE,lengthChange=FALSE,pageLength=50))
  
  output$summary_season<-renderDT(summary_season(),rownames=FALSE,options=list(scrollX=TRUE,lengthChange=FALSE,pageLength=50))
  
  output$downloadseason<-downloadHandler(
    filename=function(){paste0('Potential Points:',leaguename(),'.csv')},
    content=function(file){write.csv(summary_season(),file,row.names=FALSE)}
  )
  output$downloadweek<-downloadHandler(
    filename=function(){paste0('Potential Points:',leaguename(),'.csv')},
    content=function(file){write.csv(summary_week(),file,row.names=FALSE)}
  )
  
}

shinyApp(ui, server)