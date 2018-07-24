#' ---------
#' Title: MLB Team Statistics 
#' Subtitle: Functions for Scraping and Visualizing all Players
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### ---------------------------------------------------- ######
#'###### -------------**Scrape Function**-------------------- ######
#'###### ---------------------------------------------------- ######

# Run this function before doing the vis function!

# Function to scrape MLB team statistics
espn_team_scrape <- function(team, side, season) {
  
  # Libraries
  library(tidyverse)
  library(rvest)
  
  # URL, pasted together
  team_url <- paste0("http://www.espn.com/mlb/team/stats/", 
                     side, "/_/name/", 
                     team, "/year/", 
                     season, "/")
  
  # Building the data table
  team_tables <- team_url %>%
    read_html %>%
    html_node("#my-players-table > div.mod-container.mod-table > 
              div.mod-content > table:nth-child(1)") %>%
    html_table(header = T)
  
  team_tables$Year <- season
  
  return(team_tables)
}







