library(DBI)
library(tidyverse)
library(RSQLite)


db_local <- dbConnect(RSQLite::SQLite(),'calculator_log.sqlite')

calculator_log <- dbGetQuery(db_local,'SELECT * FROM calculator_log')

dbDisconnect(db_local)
