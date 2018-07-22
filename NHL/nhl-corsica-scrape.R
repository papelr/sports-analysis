#' ---------
#' Title: Player Statistics
#' Subtitle: Each player's statistics by season, bound together
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(readr)

#'###### -------------**CSV Read**---------------------- ######

# Set working directory:
setwd("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/player-data")

# From Corsica, session = regular season, report = summary, game state = 5v5,
  # show = all

# Player statistics 
# 2012-2013 season not included [LOCKOUT]
all_player_stats <- read.csv("player-data-2008-2018.txt")


#'###### -------------**Cleaning**---------------------- ######

# Adjusting some column names
all_player_stats <- 
  all_player_stats %>% 
  rename(Corsi_plus_minus = C...,
         CF_Perc = CF.,
         Rel_CF_Rerc = Rel.CF.,
         G_plus_minus = G...,
         GF_Perc = GF.,
         Rel_GF_Perc = Rel.GF.,
         xG_plus_minus = xG...,
         xGF_Perc = xGF.,
         Rel_xGF_Perc = Rel.xGF.,
         iP_plus_minus = iP...,
         iSh_Perc = iSh.,
         TOI_Perc = TOI.,
         TOI_Qual_of_Team = TOI..QoT,
         CF_Qual_of_Team = CF..QoT,
         TOI_Qual_of_Comp = TOI..QoC,
         CF_Qual_of_Comp = CF..QoC) 

all_player_stats <- 
  all_player_stats %>% 
  mutate(Season = recode(Season, 
                         "7-8" = "2007-2008",
                         "8-9" = "2008-2009",
                         "9-10" = "2009-2010",
                         "10-11" = "2010-2011",
                         "11-12" = "2011-2012",
                         "13-14" = "2013-2014",
                         "14-15" = "2014-2015",
                         "15-16" = "2015-2016",
                         "16-17" = "2015-2016",
                         "17-18" = "2017-2018"))

all_player_stats$Team <- gsub("\\.", "", all_player_stats$Team)

# Getting rid of repeated header lines in table
all_player_stats <- all_player_stats[all_player_stats$Player != "Player", ]

# Assign NHL team color hex codes
nhl_hex_codes <- 
  c("ANA" = "#FC4C02", "ARI" = "#8C2633", "ATL" = "#53565A",
    "BOS" = "#FFB81C", "BUF" = "#041E42", "CGY" = "#C8102E",
    "CAR" = "#CC0000", "CHI" = "#C8102E", "COL" = "#6F263D",
    "CBJ" = "#041E42", "DAL" = "#006341", "DET" = "#C8102E",
    "EDM" = "#041E42", "FLA" = "#B9975B", "LA" = "#000000",
    "MIN" = "#154734", "MON" = "#A6192E", "NJ" = "#C8102E",
    "NSH" = "#FFB81C", "NYI" = "#00468B", "NYR" = "#0038A8",
    "OTT" = "#C8102E", "PHL" = "#FA4616", "PIT" = "#CFC493",
    "STL" = "#003087", "SJ" = "#006272", "TB" = "#00205B",
    "TOR" = "#00205B", "VAN" = "#00843D", "VGK" = "#B9975B",
    "WSH" = "#041E42", "WPG" = "#53565A")
