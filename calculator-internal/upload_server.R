suppressPackageStartupMessages({
  
  library(RSQLite)
  library(odbc)
  library(DBI)
  
  library(httr)
  
  library(glue)
  library(dplyr)
  
})

upload_calculator_logs <- function(){
  
  db_local <- dbConnect(SQLite(),'calculator_log.sqlite', synchronous = "off")
  db_server <- dbConnect(odbc(),'dynastyprocess_db')
  
  on.exit({
    dbDisconnect(db_local)
    dbDisconnect(db_server)
    POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a/fail",
         body = "Failed to upload calculator logs!")
  })
  
  df_local <- dbGetQuery(db_local,'SELECT * from calculator_log')
  
  query_server <- glue_sql(
    .con = db_server,
    'SELECT trade_id FROM dp_calculatorlogs WHERE trade_id IN ({df_local$trade_id *})')
  
  df_server <- dbGetQuery(db_server,query_server)
  
  df_upload <- df_local %>% 
    anti_join(df_server, by = 'trade_id')
  
  dbAppendTable(db_server,'dp_calculatorlogs',df_upload)
  
  dbExecute(db_local,glue_sql('DELETE FROM calculator_log WHERE trade_id IN ({df_upload$trade_id*});', .con = db_local))
  
  dbExecute(db_local,glue_sql('DELETE FROM calculator_log WHERE trade_id IN ({df_server$trade_id*});', .con = db_local))
  
  dbExecute(db_local,'vacuum;')
  
  dbDisconnect(db_local)
  dbDisconnect(db_server)
  
  on.exit()

  POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a",
       body = glue("Successfully uploaded {nrow(df_upload)} rows of calculator log to server at {Sys.time()}"))
}

upload_calculator_logs()
