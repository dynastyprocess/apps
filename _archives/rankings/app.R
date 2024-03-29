library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(yaml)
library(RColorBrewer)

fantasypros<-list.files(path='/srv/data/files/fantasypros',full.names = T) %>% 
  .[length(.)] %>%
  read.csv() %>% 
  select(Player,Team,Pos,starts_with('do'),starts_with('dp')) %>% 
  filter(Pos %in% c('QB','RB','WR','TE')) %>% 
  mutate(dpSD=case_when(is.null(dpSD)~dpECR^0.6,
                        dpSD==0~dpECR^0.6,
                        dpECR>32~dpECR^0.6,
                        TRUE~dpSD))

fp_date<-list.files(path='/srv/data/files/fantasypros',full.names = T) %>% 
  .[length(.)] %>%
  str_extract("[0-9]{8}") %>% 
  as.Date('%Y%m%d')

ui <- dashboardPage(skin="blue", title="DynastyProcess Apps: Custom Rankings App", # dashboard header ----
                    dashboardHeader(title = a(href="https://dynastyprocess.com",img(src = "logo-horizontal.png",width='100%')),titleWidth = 250),
                    dashboardSidebar(width = 250, # sidebar ----
                                     sidebarMenu(
                                       menuItem("Rankings", tabName = "rank", icon = icon("chart-line")),
                                       menuItem("Summary",tabName = 'summary', icon=icon('quidditch'))
                                       )),
                    dashboardBody(tags$head( # CSS ----
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
                    tabItems( # main UI content ----
                      tabItem(tabName='rank',
                              titlePanel('Custom Ranks'),
                              fluidRow(column(9,p('Understanding how you rank a player relative to the market helps you make strategic decisions! Drag and drop to reorder ranks.'),
                              br(),
                              p('FP Ranks last updated: ',fp_date)),
                                       column(3,
                                              downloadButton('download','Download!'),
                                              actionButton('savedialog','Save to DP Database!')
                                              )),
                              br(),
                              fluidRow(tabBox(width=12,selected = 'RB', title = 'Dynasty Positional Ranks',side = 'left',
                                              tabPanel(title = 'QB',DTOutput('qb')),
                                              tabPanel(title = 'RB',DTOutput('rb')),
                                              tabPanel(title = 'WR',DTOutput('wr')),
                                              tabPanel(title = 'TE',DTOutput('te'))
                                              ))
                              ),
                      tabItem(tabName='summary',
                              titlePanel('Custom Ranks: Summary'),
                              fluidRow(column(12,p("Now that you've done the work of ranking each position, here's a few things that your data suggests!"))),
                              br(),
                              fluidRow(tabBox(width=12,side='left',
                                              tabPanel(title='Love',DTOutput('loves')),
                                              tabPanel(title='Hate',DTOutput('hates')))),
                              fluidRow(box(width=12,title = 'Overall'))
                              )
                      ))
) # end of UI ----


server <- function(input, output, session) {
  
  track_usage(storage_mode = store_sqlite(path = "logs/"))

  # qb code----
  
  qb<-fantasypros %>% 
    filter(Pos == 'QB') %>% 
    arrange(dpECR) %>%
    filter(!is.na(dpECR)) %>%
    rowid_to_column(var='Your Rank') %>%
    mutate(`Your Rank`=as.numeric(`Your Rank`),
           z = (dpECR-`Your Rank`)/dpSD) %>% 
    select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z) %>% 
    reactiveVal()
  
  num_qb<-fantasypros %>% 
    filter(Pos == 'QB',!is.na(dpECR)) %>% 
    nrow()
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))
  
  output$qb<-renderDT(server=F,{
    datatable(
      qb(),
      rownames = F,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(3, 'asc')),
                     #paging=FALSE,
                     #scrollY=600,
                     pageLength=100,
                     searching=FALSE,
                     #pagingType='simple_numbers',
                     scrollX=TRUE),
      callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('qb_row_reorder', JSON.stringify(details));});")
    ) %>% 
      formatRound(c('z','dpSD'),1) %>% 
      formatStyle(1:7,'z',backgroundColor=styleInterval(quantile(range(-3,3),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colourlist(20)))
    
  })
  
  qb_order<-reactiveVal(seq(1,num_qb))
  
  observeEvent(input$qb_row_reorder,{ # watching the "reorder" events ----
    
    qb_info<-input$qb_row_reorder
    
    if (is.null(qb_info)|class(qb_info) !='character'){return()}
    
    qb_info<-read_yaml(text=qb_info)
    
    if(length(qb_info)==0){return()}
    
    .qborder<-seq(1,num_qb)
    .new_qborder<-.qborder
    
    for (i in 1:length(qb_info)){
      j<- qb_info[[i]]
      .new_qborder[(j$oldPosition+1)]<-.qborder[(j$newPosition+1)]
    }
    
    qb_order(.new_qborder)
    
    qbdata<-qb() %>%
      select(-`Your Rank`) %>% 
      mutate(`Your Rank`=qb_order(),
             z = (dpECR-`Your Rank`)/dpSD) %>% 
      arrange(`Your Rank`) %>% 
      select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z)
    
    qb(qbdata)
    
  })
  
  # RB code----
  
  rb<-fantasypros %>% 
    filter(Pos == 'RB') %>% 
    arrange(dpECR) %>%
    filter(!is.na(dpECR)) %>%
    rowid_to_column(var='Your Rank') %>%
    mutate(`Your Rank`=as.numeric(`Your Rank`),
           z = (dpECR-`Your Rank`)/dpSD) %>% 
    select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z) %>% 
    reactiveVal()
  
  num_rb<-fantasypros %>% 
    filter(Pos == 'RB',!is.na(dpECR)) %>% 
    nrow()
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))

  output$rb<-renderDT(server=F,{
    datatable(
      rb(),
      rownames = F,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(3, 'asc')),
                     #paging=FALSE,
                     #scrollY=600,
                     pageLength=100,
                     searching=FALSE,
                     #pagingType='simple_numbers',
                     scrollX=TRUE),
      callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('rb_row_reorder', JSON.stringify(details));});")
      ) %>% 
      formatRound(c('z','dpSD'),1) %>% 
      formatStyle(1:7,'z',backgroundColor=styleInterval(quantile(range(-3,3),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colourlist(20)))
      
  })
  
  rb_order<-reactiveVal(seq(1,num_rb))
  
  observeEvent(input$rb_row_reorder,{ # watching the "reorder" events ----
    
    rb_info<-input$rb_row_reorder
    
    if (is.null(rb_info)|class(rb_info) !='character'){return()}
    
    rb_info<-read_yaml(text=rb_info)

    if(length(rb_info)==0){return()}
    
    .rborder<-seq(1,num_rb)
    .new_rborder<-.rborder
    
    for (i in 1:length(rb_info)){
      j<- rb_info[[i]]
      .new_rborder[(j$oldPosition+1)]<-.rborder[(j$newPosition+1)]
    }
    
    rb_order(.new_rborder)
    
    rbdata<-rb() %>%
      select(-`Your Rank`) %>% 
      mutate(`Your Rank`=rb_order(),
             z = (dpECR-`Your Rank`)/dpSD) %>% 
      arrange(`Your Rank`) %>% 
      select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z)
    
    rb(rbdata)
    
  })
  
  # WR code----
  
  wr<-fantasypros %>% 
    filter(Pos == 'WR') %>% 
    arrange(dpECR) %>%
    filter(!is.na(dpECR)) %>%
    rowid_to_column(var='Your Rank') %>%
    mutate(`Your Rank`=as.numeric(`Your Rank`),
           z = (dpECR-`Your Rank`)/dpSD) %>% 
    select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z) %>% 
    reactiveVal()
  
  num_wr<-fantasypros %>% 
    filter(Pos == 'WR',!is.na(dpECR)) %>% 
    nrow()
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))
  
  output$wr<-renderDT(server=F,{
    datatable(
      wr(),
      rownames = F,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(3, 'asc')),
                     #paging=FALSE,
                     #scrollY=600,
                     pageLength=100,
                     searching=FALSE,
                     #pagingType='simple_numbers',
                     scrollX=TRUE),
      callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('wr_row_reorder', JSON.stringify(details));});")
    ) %>% 
      formatRound(c('z','dpSD'),1) %>% 
      formatStyle(1:7,'z',backgroundColor=styleInterval(quantile(range(-3,3),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colourlist(20)))
    
  })
  
  wr_order<-reactiveVal(seq(1,num_wr))
  
  observeEvent(input$wr_row_reorder,{ # watching the "reorder" events ----
    
    wr_info<-input$wr_row_reorder
    
    if (is.null(wr_info)|class(wr_info) !='character'){return()}
    
    wr_info<-read_yaml(text=wr_info)
    
    if(length(wr_info)==0){return()}
    
    .wrorder<-seq(1,num_wr)
    .new_wrorder<-.wrorder
    
    for (i in 1:length(wr_info)){
      j<- wr_info[[i]]
      .new_wrorder[(j$oldPosition+1)]<-.wrorder[(j$newPosition+1)]
    }
    
    wr_order(.new_wrorder)
    
    wrdata<-wr() %>%
      select(-`Your Rank`) %>% 
      mutate(`Your Rank`=wr_order(),
             z = (dpECR-`Your Rank`)/dpSD) %>% 
      arrange(`Your Rank`) %>% 
      select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z)
    
    wr(wrdata)
    
  })
  

    
  # TE code----
  
  te<-fantasypros %>% 
    filter(Pos == 'TE') %>% 
    arrange(dpECR) %>%
    filter(!is.na(dpECR)) %>%
    rowid_to_column(var='Your Rank') %>%
    mutate(`Your Rank`=as.numeric(`Your Rank`),
           z = (dpECR-`Your Rank`)/dpSD) %>% 
    select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z) %>% 
    reactiveVal()
  
  num_te<-fantasypros %>% 
    filter(Pos == 'TE',!is.na(dpECR)) %>% 
    nrow()
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))
  
  output$te<-renderDT(server=F,{
    datatable(
      te(),
      rownames = F,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(3, 'asc')),
                     #paging=FALSE,
                     #scrollY=600,
                     pageLength=100,
                     searching=FALSE,
                     #pagingType='simple_numbers',
                     scrollX=TRUE),
      callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('te_row_reorder', JSON.stringify(details));});")
    ) %>% 
      formatRound(c('z','dpSD'),1) %>% 
      formatStyle(1:7,'z',backgroundColor=styleInterval(quantile(range(-3,3),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colourlist(20)))
    
  })
  
  te_order<-reactiveVal(seq(1,num_te))
  
  observeEvent(input$te_row_reorder,{ # watching the "reorder" events ----
    
    te_info<-input$te_row_reorder
    
    if (is.null(te_info)|class(te_info) !='character'){return()}
    
    te_info<-read_yaml(text=te_info)
    
    if(length(te_info)==0){return()}
    
    .teorder<-seq(1,num_te)
    .new_teorder<-.teorder
    
    for (i in 1:length(te_info)){
      j<- te_info[[i]]
      .new_teorder[(j$oldPosition+1)]<-.teorder[(j$newPosition+1)]
    }
    
    te_order(.new_teorder)
    
    tedata<-te() %>%
      select(-`Your Rank`) %>% 
      mutate(`Your Rank`=te_order(),
             z = (dpECR-`Your Rank`)/dpSD) %>% 
      arrange(`Your Rank`) %>% 
      select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z)
    
    te(tedata)
    
  })
  
  
  
  
  
  
  # Append ----
  
  df_allpos<-reactive({bind_rows(qb(),rb(),wr(),te())})
  
  # Love/Hates ----
  
  output$loves<-renderDT({
    df_allpos() %>% 
      filter(z>1.5) %>%
      mutate(scaling=z*10/dpECR) %>% 
      arrange(desc(scaling)) %>%
   #   select(-scaling) %>% 
      datatable(rownames=F,options(pageLength=10,scrollX=TRUE)) %>% 
      formatRound('z',1)
  })
  
  output$hates<-renderDT({
    df_allpos() %>% 
      filter(z<(-1.5)) %>%
      mutate(scaling=z*10/dpECR) %>% 
      arrange(scaling) %>%
     # select(-scaling) %>% 
      datatable(rownames=F,options(pageLength=10,scrollX=TRUE)) %>% 
      formatRound('z',1)
  })

  # save ranks to database ----
  
  observeEvent(input$savedialog,{
    showModal(modalDialog(title = 'Save to DynastyProcess Database',
                          fluidRow(column(12,
                                          p('Save your ranks to the DynastyProcess database!')
                                          )),
                          fluidRow(column(4,textInput('username',label = 'Name')),
                                   #column(8,textInput('notes',label = 'Notes'))
                                   ),
                          footer = list(actionButton('save','Save',icon=icon('save')),modalButton('Dismiss'))
                          ))
  })
  
  observeEvent(input$save,{
    saveRDS(object = df_allpos(),file = paste0('savedranks/',input$username,"_",format.Date(Sys.time(),"%Y%m%d-%H%M%S"),'.rds'))
    
    showModal(modalDialog(title='Saved!'))
    Sys.sleep(3)
    removeModal()
    
  })
  
  # DownloadHandler ----
  output$download<-
    downloadHandler(
      filename = function(){paste0('customdynastyranks_',format.Date(Sys.Date(),'%Y%m%d'),'.csv')},
      content = function(file){write.csv(df_allpos(),file,row.names = F)}
    )
} # end of server segment ----

shinyApp(ui, server)