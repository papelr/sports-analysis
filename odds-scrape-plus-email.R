#' ---------
#' Title: Odds Scraper & Auto Email
#' Subtitle: Emails out Odds Spreadsheet, NHL/NBA
#' Date: December 2018
#' Author: Robert Papel
#' ---------

#'####--------------**Libraries**--------------####

library(rvest)
library(dplyr)
library(tm)
library(stringi)
library(readr)
library(gmailr)

#'####--------------**Time/Date**--------------####

# Setting time/date
tomorrow <- Sys.Date()
tomorrow <- gsub("-", "", tomorrow, fixed = TRUE)
print(tomorrow)

#'####--------------**NHL**--------------####

# NHL url
nhl_url <- read_html(paste0('https://classic.sportsbookreview.com/betting-odds/nhl-hockey/totals/?date=', as.character(tomorrow)))

# NHL nodes
rot_nhl <- nhl_url %>%
  html_nodes('.eventLine-rotation .eventLine-book-value') %>%
  html_text()

teams_nhl <- nhl_url %>%
  html_nodes('.team-name a') %>%
  html_text()

total_nhl <- nhl_url %>%
  html_nodes('.adjust') %>%
  html_text()

opening_nhl <- nhl_url %>%
  html_nodes('.price') %>%
  html_text()

pinnacle_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(10) b') %>%
  html_text()

dimes_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(11) b') %>%
  html_text()

BookMaker_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(12) b') %>%
  html_text()

BETONLINE_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(13) b') %>%
  html_text()

BOVADA_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(14) b') %>%
  html_text()

HERITAGE_nhl <- nhl_url %>%
  html_nodes('.eventLine-book:nth-child(15) b') %>%
  html_text()

nhl_frame <- data.frame(
  ROT = rot_nhl, 
  TEAM = teams_nhl, 
  Total = total_nhl, 
  Opener = opening_nhl, 
  Pinnacle = pinnacle_nhl,
  FiveDimes = dimes_nhl, 
  Bovada = BOVADA_nhl, 
  BetOnline = BETONLINE_nhl,
  Heritage = HERITAGE_nhl, 
  BookMaker = BookMaker_nhl
  )

#'####--------------**NBA**--------------####

# NBA url
nba_url <- read_html(paste0('https://classic.sportsbookreview.com/betting-odds/nba-basketball/totals/?date=', as.character(tomorrow)))

# NBA nodes
rot_nba <- nba_url %>%
  html_nodes('.eventLine-rotation .eventLine-book-value') %>%
  html_text()

teams_nba <- nba_url %>%
  html_nodes('.team-name a') %>%
  html_text()

total_nba <- nba_url %>%
  html_nodes('.adjust') %>%
  html_text()

opening_nba <- nba_url %>%
  html_nodes('.price') %>%
  html_text()

pinnacle_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(10) b') %>%
  html_text()

dimes_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(11) b') %>%
  html_text()

BookMaker_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(12) b') %>%
  html_text()

BETONLINE_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(13) b') %>%
  html_text()

BOVADA_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(14) b') %>%
  html_text()

HERITAGE_nba <- nba_url %>%
  html_nodes('.eventLine-book:nth-child(15) b') %>%
  html_text()

nba_frame <- data.frame(
  ROT = rot_nba,
  TEAM = teams_nba, 
  Total = total_nba, 
  Opener = opening_nba, 
  Pinnacle = pinnacle_nba,
  FiveDimes = dimes_nba, 
  Bovada = BOVADA_nba, 
  BetOnline = BETONLINE_nba,
  Heritage = HERITAGE_nba, 
  BookMaker = BookMaker_nba
  )

#'####--------------**Data Frame**--------------####

# Joining NHL/NBA data frames
full_sheet <- full_join(nhl_frame, nba_frame)

# Adding "League" column to help delineate within table
# Need to recode teams above
full_sheet <- full_sheet %>% mutate(
  League = case_when(
    full_sheet$TEAM == "Arizona" ~ "NHL",
    full_sheet$TEAM == "Buffalo" ~ "NHL",
    full_sheet$TEAM == "Los Angeles" ~ "NHL",
    full_sheet$TEAM == "Columbus" ~ "NHL",
    # full_sheet$TEAM == "Toronto" ~ "NHL",
    full_sheet$TEAM == "Tampa Bay" ~ "NHL",
    full_sheet$TEAM == "Carolina" ~ "NHL",
    full_sheet$TEAM == "Montreal" ~ "NHL",
    full_sheet$TEAM == "Florida"  ~ "NHL",
    # full_sheet$TEAM == "Minnesota" ~ "NHL",
    full_sheet$TEAM == "Vancouver" ~ "NHL",
    full_sheet$TEAM == "Nashville" ~ "NHL",
    full_sheet$TEAM == "Edmonton" ~ "NHL",
    full_sheet$TEAM == "Winnipeg" ~ "NHL",
    # full_sheet$TEAM == "Dallas" ~ "NHL",
    full_sheet$TEAM == "San Jose" ~ "NHL",
    full_sheet$TEAM == "New Jersey" ~ "NHL",
    full_sheet$TEAM == "Arizona" ~ "NHL",
    full_sheet$TEAM == "N.Y. Rangers" ~ "NHL",
    # full_sheet$TEAM == "Boston" ~ "NHL", 
    full_sheet$TEAM == "Pittsburgh" ~ "NHL",
    full_sheet$TEAM == "Ottowa" ~ "NHL",
    full_sheet$TEAM == "Detroit" ~ "NHL",
    # full_sheet$TEAM == "Washington" ~ "NHL",
    full_sheet$TEAM == "Colorado" ~ "NHL",
    full_sheet$TEAM == "St. Louis" ~ "NHL",
    # full_sheet$TEAM == "Chicago" ~ "NHL",
    # full_sheet$TEAM == "Philadelphia" ~ "NHL",
    full_sheet$TEAM == "Calgary" ~ "NHL",
    full_sheet$TEAM == "Anaheim" ~ "NHL",
    full_sheet$TEAM == "Vegas" ~ "NHL",
    full_sheet$TEAM == "Houston" ~ "NBA",
    full_sheet$TEAM == "L.A. Lakers" ~ "NBA",
    full_sheet$TEAM == "L.A. Clippers" ~ "NBA",
    full_sheet$TEAM == "San Antonio"~ "NBA",
    # full_sheet$TEAM == "Chicago" ~ "NBA",
    full_sheet$TEAM == "Orlando" ~ "NBA",
    # full_sheet$TEAM == "Dallas" ~ "NBA",
    full_sheet$TEAM == "Phoenix" ~ "NBA",
    full_sheet$TEAM == "Atlanta" ~ "NBA",
    # full_sheet$TEAM == "Boston" ~ "NBA",
    full_sheet$TEAM == "New York" ~ "NBA",
    full_sheet$TEAM == "Charlotte" ~ "NBA",
    full_sheet$TEAM == "Milwaukee" ~ "NBA",
    full_sheet$TEAM == "Cleveland" ~ "NBA",
    full_sheet$TEAM == "Indiana" ~ "NBA",
    # full_sheet$TEAM == "Philadelhpia" ~ "NBA",
    # full_sheet$TEAM == "Washington" ~ "NBA",
    full_sheet$TEAM == "Brooklyn" ~ "NBA",
    full_sheet$TEAM == "Miami" ~ "NBA",
    full_sheet$TEAM == "Memphis" ~ "NBA",
    # full_sheet$TEAM == "Toronto" ~ "NBA",
    full_sheet$TEAM == "Portland" ~ "NBA",
    full_sheet$TEAM == "Golden State" ~ "NBA",
    full_sheet$TEAM == "Sacramento" ~ "NBA",
    full_sheet$TEAM == "Oklahoma City" ~ "NBA",
    full_sheet$TEAM == "Denver" ~ "NBA",
    full_sheet$TEAM == "Utah" ~ "NBA",
    full_sheet$TEAM == "Houston" ~ "NBA",
    # full_sheet$TEAM == "Minnesota" ~ "NBA",
    full_sheet$TEAM == "Detroit" ~ "NBA"
  )
)

#'####--------------**Creating CSV**--------------####

NBA_NHL_ODDS_TOTALS <- write_excel_csv(full_sheet,'NBA_NHL_ODDS_TOTALS.csv')

#'####--------------**Automated Email**--------------####

# Email to send out daily odds 
gmail_email <- mime() %>%
  to(c("robertpapel@gmail.com", 
       "Jwb154@miami.edu", 
       "connor.healey@ymail.com"
       )
     ) %>%
  from("robertpapel@gmail.com") %>%
  subject("December 14th Odds") %>% 
  text_body("-Robert P") %>% 
  attach_file("NBA_NHL_ODDS_TOTALS.csv")
send_message(gmail_email)

