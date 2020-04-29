library(DBI)
library(tidyverse)

# Get data from database ----
aws_db <- dbConnect(odbc::odbc(),"dynastyprocess_db")

players <- dbGetQuery(aws_db,"
SELECT DISTINCT

mfl_id,
fantasypros_id,
name,
position,
age,
COALESCE(fp_ecr.ecr,400) as ecr,
dp_playerids.team

FROM dp_playerids

LEFT JOIN fp_ecr ON dp_playerids.fantasypros_id = fp_ecr.id
AND fp_ecr.ecr_type = 'do'
AND fp_ecr.scrape_date = (SELECT scrape_date FROM fp_ecr ORDER BY scrape_date desc limit 1)

WHERE position in ('QB','RB','WR','TE')
ORDER BY ecr

                    ")

write_parquet(players,"players.pdata")

pick_values <- dbGetQuery(aws_db,"SELECT * FROM dp_rookiepickvalues")

write_parquet(pick_values,"rookie_values.pdata")

dbDisconnect(aws_db)
