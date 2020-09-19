suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(RSQLite)
  library(here)
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)
})

source('../calculator-internal/fn_ui_desktop.R')

#import data
fp_ecr <- read_csv("https://raw.githubusercontent.com/DynastyProcess/data/master/files/db_fpecr.csv") %>%
  filter(ecr_type %in% c('dp','rp'), pos %in% c('QB','RB','WR','TE')) %>%
  select(player_name, fantasypros_id, pos, team, scrape_date, ecr, ecr_type) %>%
  pivot_wider(names_from = ecr_type,
              values_from = ecr,
              values_fn = min) %>%
  ungroup() %>%
  transmute(name = as.factor(player_name),
            pos = as.factor(pos),
            tm = as.factor(team),
            date = as.factor(scrape_date),
            dynpECR = dp,
            rdpECR = rp) %>%
  arrange(date, dynpECR)

dates <- fp_ecr %>% distinct(date) %>% pull()
names <- fp_ecr %>% distinct(name) %>% pull()


#Custom Table Container
createContainer <- function(dates){
  
  for (i in length(dates))
  {thString <- paste0('th(colspan = 2, dates[',i,'], style="text-align:center",')}
  
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Player'),
        th(colspan = 2, dates[1], style="text-align:center"),
        th(colspan = 2, dates[2], style="text-align:center"),
        th(colspan = 2, dates[3], style="text-align:center")
        
      ),
      tr(
        lapply(rep(c('Dynasty', 'Redraft'), 3), th)
      )
    )
  ))
}


ui <- dashboardPage(
  dashboardSidebar(disable = TRUE),
  navbar = ui_header(),  
  dashboardBody(
    hr(),
    box(title = "About",
        status = "danger",
        width = 12,
        p("Use the ECR Explorer to examine trends in FantasyPros redraft and dynasty positional ranks,
          and to identify win-now or rebuild targets. To zoom in on a specific area, click & drag to
          select the area, then double-click to focus. Double-click to reset.")),
    br(),
    box(title = "Inputs",
        status = "danger",
        width = 12,
        fluidRow(
          column(2,
                 radioGroupButtons("posFilter", "Position:",
                                   choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
                                   selected = "QB",
                                   status = "danger",
                                   justified = TRUE)),
          column(3,
                 sliderInput("playerRange",
                             "Filter ECR Range:",
                             min = 1,
                             max = 64,
                             width = '100%',
                             value = c(1,24)
                 )),
          column(4,
                 sliderTextInput("DateRange",
                                 "Select Date Range:",
                                 width = '100%',
                                 choices = dates,
                                 selected = fp_ecr %>% distinct(date) %>% slice(c(n()-6,n())) %>% pull()
                 )
          ),
          column(3,
                 selectizeInput("playerList",
                                "Select Players",
                                choices = names,
                                selected = NULL,
                                width = '100%',
                                multiple = TRUE),
                 actionButton("clear1",
                              "Clear Selected Players",
                              icon = icon("trash"),
                              class="btn radiobtn btn-success"))
        )
    ),
    br(),
    fluidRow(
      box(title = "Plot",
          status = "danger",
          width = 6,
          plotOutput("distPlot",
                     height = "800px",
                     #width = "100%",
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
      box(title = "Table",
          status = "danger",
          width = 6,
          div(DTOutput("printData")), style='font-size: small',
          br(),
          downloadButton("downloadData1", "Download", class="btn radiobtn btn-success"))

    )
  )
)

server <- function(input, output, session) {

  dateList <- reactive({
    fp_ecr %>%
      distinct(date) %>%
      slice(c(which(date == input$DateRange[1]),
              ceiling(mean(c(which(date == input$DateRange[1]), 
                             which(date == input$DateRange[2])))),
              which(date == input$DateRange[2]))) %>%
      pull()
  })
  
  dfPos <- reactive({
    fp_ecr %>%
      filter(pos == input$posFilter,
             date %in% dateList())
  })
  
  df <- reactive({
    dfPos() %>%
      filter(between(dynpECR, input$playerRange[1], input$playerRange[2]),
             date %in% dateList()) %>%
      {if (!is.null(input$playerList)) filter(., name %in% input$playerList) else .} %>%
      arrange(desc(as.Date(date)), dynpECR)
    
  })
  
  dfwide <- reactive({
    df() %>%
      rename(D = dynpECR,
             R = rdpECR) %>%
      select(name, date, D, R) %>%
      pivot_wider(names_from = date,
                  values_from = c(D, R)) %>%
      relocate(c(1,2,5,3,6,4,7))

  })
  
  observeEvent(input$clear1, {
    shinyjs::reset("options")
  })
  
  observeEvent({input$posFilter
    input$DateRange},
    {
      currentA <- input$playerList
      dates <- dateList()
      updateSelectizeInput(session,
                           'playerList',
                           choices = unique(dfPos()["name"]),
                           selected = currentA
      )
    })
  
  # observeEvent({input$posFilter
  #   input$DateRange},
  #   {
  #     names <- unique(dfPos()["name"])
  #     dates <- dateList()
  #     updateSliderInput(session, 'playerRange',
  #                       max = nrow(names))
  #   })

  output$printData <- renderDT({
    DT::datatable(dfwide() ,
                  options = list(
                    pageLength = 25,
                    order = list(5,'asc'),
                    autoWidth = TRUE
                  ),
                  class = 'compact stripe',
                  rownames= FALSE,
                  container = createContainer(dateList()))
  })
   

  output$downloadData1 <- downloadHandler(
    filename = function() {"DynastyProcessECR.csv"},
    content = function(file) {write_csv(dfwide(), file)}
  )

  # output$printData2 <- renderDT({ x },
  #                               filter='top',
  #                               options = list(pageLength = 25,
  #                                              order=list(list(3,'desc'),list(4,'asc')),
  #                                              autoWidth = TRUE
  #                               ),
  #                               class = 'compact stripe',
  #                               rownames= FALSE
  # )

  # output$downloadData2 <- downloadHandler(
  #   filename = function() {"DynastyProcessAllECR.csv"},
  #   content = function(file) {write.csv(x, file)}
  # )

  ranges <- reactiveValues(xcoord = NULL, ycoord = NULL)
  xrange <- reactiveValues(x1 = 0, x2 = 220)
  yrange <- reactiveValues(y1 = 0, y2 = 220)

  output$distPlot <- renderPlot({
    if (is.infinite(max(ranges$xcoord,ranges$ycoord)))
    {rangeVec <- c(min(df()$dynpECR,df()$rdpECR),max(df()$dynpECR,df()$rdpECR))}
    else {rangeVec <- c(min(ranges$xcoord,ranges$ycoord),max(ranges$xcoord,ranges$ycoord))}

    while(rangeVec[2] - rangeVec[1] < 15)
    {if(rangeVec[1] <= 8)
    {rangeVec <- c(0,16)}
      else {
        rangeVec[1] <- rangeVec[1] - 1
        rangeVec[2] <- rangeVec[2] + 1 }
    }

    dates <- dateList()

    sizes <- 4:(length(dates)+3)

    ggplot(df(), aes(dynpECR, rdpECR, group=name)) +
      geom_point(aes(color=date, size=date)) +
      scale_size_manual( values = sizes) +
      geom_path() +
      geom_abline() +
      scale_color_brewer(palette="Set1") +
      geom_text_repel(force = 10,
                      data = . %>%
                        mutate(label = ifelse(df()$date == tail(dates, 1) &
                                                xrange$x1 <= df()$dynpECR &
                                                df()$dynpECR  <= xrange$x2 &
                                                yrange$y1 <= df()$rdpECR &
                                                df()$rdpECR <= yrange$y2,
                                              as.character(df()$name), "")),
                      aes(label = label),
                      box.padding = 0.5,
                      segment.color = "grey50") +
      theme_light() +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold"),
            legend.title = element_text(size=16),
            legend.text = element_text(size=14),
            legend.position="bottom") +
      xlab("Dynasty ECR") +
      ylab("Redraft ECR") +
      labs(color = "Dates", size = "Dates") +
      #expand_limits(x = c(0, max(16, ranges$xcoord)), y = c(0, max(16, ranges$ycoord))) +
      annotation_custom(textGrob("Win Now",x=0.95, y=0.1, hjust=1, vjust=0,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
      annotation_custom(textGrob("Dynasty Darlings",x=0.05, y=0.9, hjust=0, vjust=1,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
      #geom_point_interactive(aes(tooltip = name)) +
      coord_fixed(ratio = 1, xlim = rangeVec, ylim = rangeVec, expand = TRUE)
    #xlim(0, defaultSizelocal) +
    #ylim(0, defaultSizelocal) +
    #coord_cartesian(xlim = ranges$xcoord, ylim = ranges$ycoord, expand = TRUE)
    #coord_equal() +

  })

  observeEvent(input$dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$xcoord <- c(brush$xmin, brush$xmax)
      ranges$ycoord <- c(brush$ymin, brush$ymax)
      xrange$x1 <- brush$xmin
      xrange$x2 <- brush$xmax
      yrange$y1 <- brush$ymin
      yrange$y2 <- brush$ymax

    } else {
      ranges$xcoord <- NULL
      ranges$ycoord <- NULL
      xrange$x1 <- 0
      xrange$x2 <- 220
      yrange$y1 <- 0
      yrange$y2 <- 220
    }
  })


  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- ((hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left))# - 0.3
    top_pct <- ((hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom))# + 0.28

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px +2, "px; top:", top_px +2, "px; padding: 0;")


    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })

  output$hover_info2 <- renderUI({
    hover <- input$plot_click
    point <- nearPoints(df(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) #- 0.05
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) #+ 0.05

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; ; z-index:75; background-color: rgba(245, 245, 245, 0.70); ",
                    "left:", left_px +2, "px; top:", top_px +2 , "px; padding: 0;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })
  
}

shinyApp(ui, server)
  