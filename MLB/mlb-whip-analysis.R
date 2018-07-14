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
get_whip_data <- function(season) {
  
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
  yr_df <- rbind(a_data, b_data, c_data)   
  yr_df$year <- yr_param
  
  return(yr_df)
}

df_list <- lapply(2005:2017, get_whip_data)

final_df <- do.call(rbind, df_list)  

# Adjusting rows
names(whip_2018) <- as.matrix(whip_2018[1, ])
whip_2018 <- whip_2018[-1, ]
whip_2018 <- whip_2018[c(-11,-22,-33,-44,-55,-66,-77,-88), ]
whip_2018$"YEAR" <- 2018 
whip_2018[] <- lapply(whip_2018, function(x) type.convert(as.character(x)))

whip_2018 <- whip_2018 %>% 
  select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, WAR, WHIP, ERA)