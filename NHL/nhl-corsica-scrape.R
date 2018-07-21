#' ---------
#' Title: Player Statistics
#' Subtitle: Each player's statistics by season, bound together
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(readr)
library(clipr)

#'###### -------------**CSV Copy & Save**---------------------- ######

# Set working directory:
setwd("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/player-data")

# From Corsica, session = regular season, report = summary, game state = 5v5,
  # show = all

# Player statistics 
player_stats_2008 <- read.csv("player-data-2008.txt")
player_stats_2009 <- read.csv("player-data-2009.txt")
player_stats_2010 <- read.csv("player-data-2010.txt")
player_stats_2011 <- read.csv("player-data-2011.txt")
player_stats_2012 <- read.csv("player-data-2012.txt")
player_stats_2014 <- read.csv("player-data-2014.txt")
player_stats_2015 <- read.csv("player-data-2015.txt")
player_stats_2016 <- read.csv("player-data-2016.txt")
player_stats_2017 <- read.csv("player-data-2017.txt")
player_stats_2018 <- read.csv("player-data-2018.txt")

# 2012-2013 season [LOCKOUT, so, shortened & ignored]

# Loop to standardize column names
for (i in 1:length()) {
  
 %>% 
  rename(corsi_plus_minus = C...,
         cf_perc = CF.,
         rel_cf_perc = Rel.CF.,
         g_plus_minus = G...,
         gf_perc = GF.,
         rel_gf_perc = Rel.GF.,
         xg_plus_minus = xG...,
         xgf_perc = xGF.,
         rel_xgf_perc = Rel.xGF.,
         ip_plus_minus = iP...,
         ish_perc = iSh.,
         toi_perc = TOI.,
         toi_qual_of_team = TOI..QoT,
         cf_qual_of_team = CF..QoT,
         toi_qual_of_comp = TOI..QoC,
         cf_qual_of_comp = CF..QoC)
}


#'###### -------------**Binding**---------------------- ######

# Bind all the player statistic tables
all_player_stats <- bind_rows(player_stats_2008, player_stats_2009,
                              player_stats_2010, player_stats_2011,
                              player_stats_2012, player_stats_2014,
                              player_stats_2015, player_stats_2016,
                              player_stats_2017, player_stats_2018)


#'###### -------------**Clean-up**---------------------- ######

# Replacing Season abbreviations with ending year of season
all_nhl_player_stats$Season <- sub("7-8", "2008", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("8-9", "2009", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("9-10", "2010", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("10-11", "2011", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("11-12", "2012", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("13-14", "2014", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("14-15", "2015", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("15-16", "2016", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("16-17", "2017", all_nhl_player_stats$Season)
all_nhl_player_stats$Season <- sub("17-18", "2018", all_nhl_player_stats$Season)

