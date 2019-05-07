# Title: Scraping Odds -----
# Subtitle: Using RSelenium
# Date: Summer 2019
# Author: Robert Papel

### Libraries -----

library(tidyverse)
library(RSelenium) # using Docker
library(rvest)
library(httr)

## RSelenium setup -----
remDr <- remoteDriver(port = 4445L, browserName = "chrome")
remDr$open()

## Navigate to Bovada (MLB) ----- 
remDr$navigate("https://www.bovada.lv/sports/baseball/mlb")

## Get text from MLB betting odds page -----
all_text_raw <- remDr$findElement(using = "css", '.grouped-events , .period')
all_text <- dates_raw$getElementText()

## Separating out text -----
dates <- str_extract_all(all_text, '[1-9]/[1-99]/[1-9][1-9]')
times <- str_extract_all(all_text, 
                         '((1[0-2]|0?[1-9]):([0-5][0-9]) ([AaPp][Mm]))')
away_team <- 
home_team <- 



 


## Testing -----

url <- "https://www.bovada.lv/sports/baseball/mlb"

url %>% 
  read_html() %>% 
  html_nodes('') %>% 
  html_text


remDr$getCurrentUrl()
remDr$screenshot(display = TRUE)