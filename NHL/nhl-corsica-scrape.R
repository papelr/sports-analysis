#' ---------
#' Title: 
#' Subtitle: 
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(rvest)

#'###### -------------**Scrape**---------------------- ######

# Data from Corsica
corsica_url <- "https://www.corsicahockey.com/nhl/players/nhl-player-stats"

Right  <- corsica_url %>%
  read_html %>%
  html_node("#post-116 > div > div.actions-header > div.player-stats-table-wrap.table-scroll.active > div > table:nth-child(1)") %>%
  html_table(header = T) 
                         
                         
                        