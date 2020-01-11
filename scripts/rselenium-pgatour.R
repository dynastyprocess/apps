library(rvest)
library(tidyverse)
library(RSelenium)

rD <- rsDriver(verbose = FALSE,port=4567L,browser = "firefox")

remDr <- rD$client

url<-'https://www.pgatour.com/tournaments/the-american-express/field.html'

remDr$open()

remDr$navigate(url)

page <- read_html(remDr$getPageSource()[[1]]) %>% 
  html_nodes(xpath = "//select[@class='hasCustomSelect']") %>% 
  html_children()

df<-tibble(player = html_text(page))

remDr$close()

rD$server$stop()
