#' ---------
#' Title: 
#' Subtitle: 
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(readr)
library(clipr)

#'###### -------------**CSV Copy & Save**---------------------- ######

# Corsica lets one copy/paste csv data: read and saved in wd() here:
nhl <- read.csv(text = clipr::read_clip())
write_csv(nhl, "corsica-all-player-data.csv") 

#'###### -------------**Scrape**---------------------- ######

                        