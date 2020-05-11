suppressPackageStartupMessages({
  
  library(DBI)
  library(RSQLite)
  library(odbc)
  library(glue)
  
})

db_local <- dbConnect(SQLite(),'calculator_log.sqlite')
db_server <- dbConnect(odbc(),'dynastyprocess_db')

df_server <- dbGetQuery(db_server,'SELECT trade_id FROM dp_calculatorlogs')

df_local <- dbGetQuery(db_local,'SELECT * from calculator_log')

df_upload <- df_local %>% 
  anti_join(df_server,by = 'trade_id')

dbAppendTable(db_server,'dp_calculatorlogs',df_upload)
dbExecute(db_local,glue_sql('DELETE FROM calculator_log WHERE trade_id IN ({df_upload$trade_id*});', .con = db_local))
dbExecute(db_local,'vacuum;')

dbDisconnect(db_local)
dbDisconnect(db_server)

message(glue("Successfully uploaded {nrow(df_upload)} rows of calculator log to server at {Sys.time()}"))

