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
espn_team_stats <- function(side, team, season) {
  
  # Libraries
  library(tidyverse)
  library(rvest)
  
  # URL vector
  team_url <- paste0("http://www.espn.com/mlb/team/stats/", side, "/_/name/",
                     team, "/year/", season, "/", collapse = ",")

  # Building the data table
  team_page <- team_url %>%
    read_html %>%
    html_node("#my-players-table > div.mod-container.mod-table > 
              div.mod-content > table:nth-child(1)") %>%
    html_table(header = T)
  
  team_tables <- team_page
  team_tables$Year <- c(team, season, side)
  
  return(espn_team_stats)
}

# Uses the function to pull in 2000-2018 seasons
applying <- lapply(2000:2018, espn_team_stats)
final_team_stats <- do.call(rbind, applying)  