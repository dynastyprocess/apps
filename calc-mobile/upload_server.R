suppressPackageStartupMessages({
  
  library(DBI)
  library(RSQLite)
  library(odbc)
  
})

db_local <- dbConnect(SQLite(),'calculator_log2.sqlite')
db_server <- dbConnect(odbc(),'dynastyprocess_db')

df <- dbGetQuery(db_local,'SELECT * from calculator_log')

dbAppendTable(db_server,'dp_calculatorlogs',df)
dbExecute(db_local,glue_sql('DELETE FROM calculator_log WHERE trade_id IN ({df$trade_id*});', .con = db_local))
dbExecute(db_local,'vacuum;')

dbDisconnect(db_local)
dbDisconnect(db_server)

message(glue("Successfully uploaded {nrow(df)} rows of calculator log to server at {Sys.time()}"))
