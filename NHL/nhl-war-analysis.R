#' ---------
#' Title: How WAR Changes with Age
#' Subtitle: NHL - Wins Above Replacement
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(rvest)

#'###### -------------**Scrape**---------------------- ######

fil_link <- "https://www.hockey-reference.com/players/f/forsbfi01.html"

fil_stats <- NULL

for (i in 1:length(fil_link)) {
  
  fil_standard <- fil_link[i] %>%
    read_html %>%
    html_node("#stats_basic_plus_nhl") %>%
    html_table(header = T)
  
  fil_stats <- rbind(fil_standard, fil_pos)
  
}

