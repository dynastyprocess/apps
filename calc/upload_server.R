suppressPackageStartupMessages({
  
  library(DBI)
  library(RSQLite)
  library(odbc)
  
})

db_local <- dbConnect(SQLite(),'calculator_log2.sqlite')
db_server <- dbConnect(odbc(),'dynastyprocess_db')

df <- dbGetQuery(db_local,'SELECT * from calculator_log')

dbAppendTable(db_server,'dp_calculatorlogs',df)
dbExecute(db_local,'DELETE FROM calculator_log;')

dbDisconnect(db_local)
dbDisconnect(db_server)

message(paste("Successfully uploaded calculator log to server at",Sys.time()))