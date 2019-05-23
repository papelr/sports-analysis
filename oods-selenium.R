# Title: Scraping MLB Odds -----
# Subtitle: Using RSelenium
# Date: Summer 2019
# Author: Robert Papel

### Libraries -----

library(tidyverse)
library(RSelenium) # using Docker
library(rvest)
library(httr)

### RSelenium setup -----
remDr <- remoteDriver(port = 4445L, browserName = "chrome")
remDr$open()

### Navigate to Bovada (MLB) ----- 
remDr$navigate("https://www.bovada.lv/sports/baseball/mlb")

## Get text from MLB betting odds page -----
all_text_raw <- remDr$findElement(using = "css", '.grouped-events , .period')
all_text <- all_text_raw$getElementText()

### Separating out elements -----

## Date (work in progress)
# dates <- unlist(str_extract_all(all_text, '[1-9]/[1-99]/[1-9][1-9]'))

## Time (work in progress)
times_raw <- remDr$findElements( using = "css", 'body > bx-site > ng-component > div > sp-main > div > main > div > section > main > sp-path-event > div > sp-next-events > div > div > div > sp-coupon:nth-child(9) > sp-multi-markets > section > section > sp-score-coupon > span')
times_loop <- sapply(times_raw, function(all) {
  all$getElementText()
})
time <- unlist(times_loop)

## Away Team
away_team_raw <- remDr$findElements(
  using = "css", '.competitor-name:nth-child(1) .name')
away_team_loop <- sapply(away_team_raw, function(all) {
  all$getElementText()
})
away_team <- unlist(away_team_loop)

## Home Team
home_team_raw <- remDr$findElements(
  using = "css", '.horizontal+ .competitor-name .name')
home_team_loop <- sapply(home_team_raw, function(all) {
  all$getElementText()
})
home_team <- unlist(home_team_loop)

## Away Runline Spread
runline_spread_raw_away <- remDr$findElements(
  using = "css", 'li:nth-child(1) sp-spread-outcome .bet-handicap')
runline_spread_loop_away <- sapply(runline_spread_raw_away, function(all) {
  all$getElementText()
})
runline_spread_away <- unlist(runline_spread_loop_away)

## Away Runline Odds
runline_line_raw_away <- remDr$findElements(
  using = "css", 'li:nth-child(1) sp-spread-outcome+ .bet-price')
runline_line_loop_away <- sapply(runline_line_raw_away, function(all) {
  all$getElementText()
})                    
runline_line_away <- unlist(runline_line_loop_away)

## Home Runline Spread
runline_spread_raw_home <- remDr$findElements(
  using = "css", 'li+ li sp-spread-outcome .bet-handicap')
runline_spread_loop_home <- sapply(runline_spread_raw_home, function(all) {
  all$getElementText()
})
runline_spread_home <- unlist(runline_spread_loop_home)

## Home Runline Odds
runline_line_raw_home <- remDr$findElements(
  using = "css", 'li+ li sp-spread-outcome+ .bet-price')
runline_line_loop_home <- sapply(runline_line_raw_home, function(all) {
  all$getElementText()
})                    
runline_line_home <- unlist(runline_line_loop_home)

## Away Money Line
away_ML_raw <- remDr$findElements(
  using = "css", 'li:nth-child(1) .bet-price:nth-child(1)')
away_ML_loop <- sapply(away_ML_raw, function(all) {
  all$getElementText()
})                    
away_ML <- unlist(away_ML_loop)

## Home Money Line
home_ML_raw <- remDr$findElements(
  using = "css", 'li+ li .bet-price:nth-child(1)')
home_ML_loop <- sapply(home_ML_raw, function(all) {
  all$getElementText()
})                    
home_ML <- unlist(home_ML_loop)

## Over/Under Numbers (OVER)
overs_raw <- remDr$findElements(
  using = "css", 'li:nth-child(1) .both-handicaps')
overs_loop <- sapply(overs_raw, function(all) {
  all$getElementText()
})
over <- overs_loop[c(-1,-3,-5,-7,-9,-11,-13,-15,-17,-19,-21,-23,-25,-27,-29)]
over <- unlist(over)

## Over/Under Odds (OVER)
over_odds_raw <- remDr$findElements(
  using = "css", 'li:nth-child(1) sp-total-outcome+ .bet-price')
over_odds_loop <- sapply(over_odds_raw, function(all) {
  all$getElementText()
})                    
over_odds <- unlist(over_odds_loop)

## Over/Under Numbers (UNDER)
under_raw <- remDr$findElements(
  using = "css", 'li+ li .both-handicaps')
under_loop <- sapply(overs_raw, function(all) {
  all$getElementText()
})
under <- under_loop[c(-1,-3,-5,-7,-9,-11,-13,-15,-17,-19,-21,-23,-25,-27,-29)]
under <- unlist(under)

## Over/Under Odds (UNDER)
under_odds_raw <- remDr$findElements(
  using = "css", 'li+ li sp-total-outcome+ .bet-price')
under_odds_loop <- sapply(under_odds_raw, function(all) {
  all$getElementText()
})                    
under_odds <- unlist(over_odds_loop)

## Creating the data frame
bov_table <- tibble(away_team, home_team,
                    runline_spread_away, runline_line_away,
                    runline_spread_home, runline_line_home,
                    away_ML, home_ML, over, over_odds, under, under_odds)
 






## Testing -----

url <- "https://www.bovada.lv/sports/baseball/mlb"

url %>% 
  read_html() %>% 
  html_nodes('body > bx-site > ng-component > div > sp-main > div > main > div > section > main > sp-path-event > div > sp-next-events > div > div > div > sp-coupon:nth-child(2) > sp-multi-markets > section > section > sp-outcomes > sp-two-way-vertical:nth-child(1) > ul > li:nth-child(1) > sp-outcome > button > span.bet-price') %>% 
  html_text


remDr$getCurrentUrl()
remDr$screenshot(display = TRUE)
span.bet-price

#old time extractor
unlist(str_extract_all(all_text, 
                       '((1[0-2]|0?[1-9]):([0-5][0-9]) ([AaPp][Mm]))'))