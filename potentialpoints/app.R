source("functions.R")
options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "ESPN Potential Points - DynastyProcess.com",
  navbar = ui_header("ESPN Potential Points - DynastyProcess.com"),
  sidebar = ui_sidebar(
    menuItem("ESPN Potential Points", tabName = "espnpp", icon = "hat-wizard"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch")
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "espnpp",
        fluidRow(
          box_inputs(),
          box_about()
        ),
        uiOutput('summary_data')
      )
    )
  )
)

server <- function(input, output, session) {
 
  sever_dp()

  #### Server-side UI ####
  
  output$download_button <- renderUI({
    req(input$load)
    downloadButton('download_data',label = 'Download to Excel')
  })
  
  output$summary_data <- renderUI({
    req(input$load)
    box_summary()
  })

  #### Get League Info ####
  
  league_info <- eventReactive(
    input$load,
    get_leagueinfo(input$league_id,input$year_select,input$week_select)
    )
  
  #### Calculate Potential Points ####
  
  pp_details <- eventReactive(
    league_info(),
    {
      tibble(
        league_id = input$league_id,
        year = input$year_select,
        scoreweek = c(input$week_select[1]:league_info()$max_week)) %>% 
        mutate(lineups = pmap(list(league_id,year,scoreweek),get_potentialpoints)) %>% 
        unnest(lineups) %>% 
        left_join(league_info()$users, by = 'team_id') %>% 
        select(Week=week,
               User=user_name,
               Team=team_name,
               Pos=pos,
               ActualScore=score,
               Player=player,
               Points=points)
      }
  )
  
  summary_week <- reactive(
    pp_details() %>% 
      group_by(User,Team, Week) %>% 
      summarise(ActualScore = mean(ActualScore,na.rm = TRUE),
                PotentialScore = sum(Points,na.rm = TRUE)) %>% 
      ungroup()
  )
  
  summary_season <- reactive(
    
    summary_week() %>% 
      group_by(User,Team) %>%
      summarise(ActualScore = sum(ActualScore),
                PotentialScore = sum(PotentialScore)) %>% 
      ungroup() %>% 
      arrange(desc(PotentialScore))
    
  )
  
  #### Display Potential Points ####
  
  output$details <- renderDT(
    pp_details(),
    rownames = FALSE,
    options = list(
      scrollX = TRUE, 
      pageLength = 25))
  
  output$summary_week <- renderDT(
    summary_week(),
    rownames = FALSE,
    options = list(
      scrollX = TRUE, 
      pageLength = 25)
  )
  
  output$summary_season <- renderDT(
    summary_season(),
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 25)
  )
  
  output$download_data <- downloadHandler(
    filename = function(){paste0('PotentialPoints_',league_info()$league_name,'.xlsx')},
    content = function(file){
      write_xlsx(
        list(
          league_info = 
            enframe(league_info()) %>% 
            filter(name!='users') %>% 
            mutate(value = as.character(value)),
          summary_season = summary_season(),
          summary_week = summary_week(),
          details = pp_details()
        ),
        path = file)
      
    }
  )

  observeEvent(
    input$debug_please,
    showModal(
      modalDialog(
        title = "Having trouble?",
        "Open this page in an incognito window to see if the app is reading the ESPN API correctly!",
        br(),
        glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{input$year_select}/segments/0/leagues/{input$league_id}?view=mSettings"),
        easyClose = TRUE,
        size = 'l'
      )
    )
  )
}

shinyApp(ui, server)