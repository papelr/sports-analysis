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
  
  # Using expand.grid() to run all combinations of the links above
  # url_factors <- expand.grid(side = c("batting", "fielding"), 
  #                            team = c("ari", "atl", "bal", "bos", "chc", 
  #                                     "chw", "cws","cin", "cle", "det", "fla",                                        "mia", "hou", "kan","laa", "lad", 
  #                                     "mil", "min", "nyy", "nym", "oak", "phi",                                       "pit", "sd", "sf", "sea", 
  #                                     "stl", "tb", "tex", "tor", "was", "wsh"), 
  #                            season = c(2002, 2003, 2004, 2005, 2006, 2007, 
  #                                       2008, 2009,2010, 2011, 2012, 2013, 2014,                                         2015, 2016, 2017, 2018))
  
  team_tables <- rbind(team, side, season)
  team_tables$Year <- season
  
  return(team_tables)
}

stat_table <- lapply(2002:2018, espn_team_scrape)







