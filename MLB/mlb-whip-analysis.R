#' ---------
#' Title: How WHIP Changes with Age
#' Subtitle: MLB - WHIP Stat
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(rvest)

#'###### -------------**Scrape**---------------------- ######

# A function that creates a "year" parameter within the ESPN links
espn_pitching_stats <- function(season) {
  
  # Creating the URL vectors, without the year
  whip_a <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/order/false")
  whip_b <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/count/41/qualified/true/order/false")
  whip_c <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/count/81/order/false")
  
  # Building the data tables, ignores the 3rd if only two pages in table
  a_data <- whip_a %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  b_data <- whip_b %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  c_data <- whip_c %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  # Binding all the tables together
  pitching_tables <- rbind(a_data, b_data, c_data)   
  pitching_tables$Year <- season
  
  return(pitching_tables)
}

# Uses the function to pull in 2000-201_ seasons
whip_table <- lapply(2000:2017, espn_pitching_stats)
final_table <- do.call(rbind, whip_table)  

# Adjusting column headers
names(final_table) <- as.matrix(final_table[1, ])
final_table <- final_table[-1, ]

# Getting rid of inner headers - the RK PLAYER TEAM, etc.
final_table <- final_table[final_table$RK != "RK", ]
final_table[] <- lapply(final_table, function(x) type.convert(as.character(x), 
                                                      as.is = TRUE))

# Selecting specific variables
whip_2018 <- whip_2018 %>% 
  select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, WAR, WHIP, ERA)