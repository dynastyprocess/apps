####  Server functions ####
suppressPackageStartupMessages({

  # Data import/export
  library(pool)
  library(DBI)
  library(RSQLite)
  library(arrow)
  library(rvest)
  
  # Data manip
  library(dplyr)
  library(glue)
  
  # Shiny
  library(shiny)
  library(sever) # github::johncoene/sever
  library(joker) # github::tanho63/joker

})

sever_joke <- function(){ 
  
  sever::sever(
    shiny::tagList(
      shiny::h1("Disconnected"),
      shiny::p(shiny::em(try(joker::randomjoke()))),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
}

platform <- 'MFL'
leagueID <- '12345'
draftID <- NULL
community <- 'r/DynastyFF'

conn <- dbConnect(SQLite(),'leaguelist_adp.sqlite')

validate_inputs <- function(db_pool,platform,leagueID,draftID,community){
  
  conn <- poolCheckout(db_pool)
  on.exit(poolReturn(conn),add = TRUE)
  
  Sys.sleep(2) # enforce a minimum 2 second wait time to reduce spammy behaviours?
  
  local_check <- validate_local(conn,platform,leagueID,draftID) 
  # Return OK if not in database, return DUPLICATE if in database
  
  if(local_check == 'DUPLICATE'){
    return(glue("Looks like we've already got {leagueID}{draftID} from {platform} in our database!"))}
  
  api_check <- switch(platform,
                      "Sleeper" = validate_sleeper(draftID),
                      "MFL" = validate_mfl(leagueID)
                      ) 
  # Return "OK" if HTTP request is OK, return "ERROR" if HTTP request has an error 
  
  if(api_check == "ERROR"){
    return(glue("We had an error getting {leagueID}{draftID} from the {platform} API. 
                Please check your inputs and try again!"))}
  
  df_save <- tibble(platform = platform,
                    submission_time = Sys.time(),
                    mfl_leagueid = leagueID,
                    sleeper_draftid = draftID,
                    community_tag = community)
  
  dbWriteTable(conn,'rookieadp_leaguelist',df_save,append = TRUE)
  # Save to local SQLite file, then up it to AWS each day and run a job to pull the actual ADP results later
  
  return(glue("Successfully added {leagueID}{draftID} from {platform} to our database. 
              We'll be recompiling the database every day, 
              so expect to see your league results in a little bit!"))
}

validate_local <- function(conn,platform,leagueID,draftID){
  # Check local database to see if platform/leagueID already exist in local database
  
  df_matching <- dbGetQuery(conn,glue_sql("SELECT * FROM rookieadp_leaguelist 
                           WHERE platform = {platform}
                           AND leagueID = {coalesce(leagueID,'%')}
                           AND draftID = {coalesce(draftID,'%')}
                           ",.con = conn)) %>% 
    nrow()
  
}

validate_sleeper <- function(draftID){
  # Check Sleeper draft pull to see if draftID exists
  
}

validate_mfl <- function(leagueID){
  # Check MFL league to see if leagueID exists
  
}

