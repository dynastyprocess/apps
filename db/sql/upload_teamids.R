library(tidyverse)
library(DBI)

team_ids <- read_csv('teamIDs.csv')

aws_db <- dbConnect(odbc::odbc(),'dynastyprocess_db')

# dbWriteTable(aws_db,'dp_teamids',team_ids,overwrite = TRUE)

dbExecute(aws_db,'TRUNCATE TABLE dp_teamids')

dbAppendTable(aws_db,'dp_teamids',team_ids)

dbDisconnect(aws_db)