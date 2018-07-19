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
setwd("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL")

# From Corsica, session = regular season, report = individual, game state = 5v5,
  # show = all

# 2007-2008 season:
player_stats_2008 <- read.csv(text = clipr::read_clip())

# 2008-2009 season:
player_stats_2009 <- read.csv(text = clipr::read_clip())

# 2009-2010 season:
player_stats_2010 <- read.csv(text = clipr::read_clip())

# 2010-2011 season:
player_stats_2011 <- read.csv(text = clipr::read_clip())

# 2011-2012 season:
player_stats_2012 <- read.csv(text = clipr::read_clip())

# 2012-2013 season [LOCKOUT]:
player_stats_2013 <- read.csv(text = clipr::read_clip())

# 2013-2014 season:
player_stats_2014 <- read.csv(text = clipr::read_clip())

# 2014-2015 season:
player_stats_2015 <- read.csv(text = clipr::read_clip())

# 2015-2016 season:
player_stats_2016 <- read.csv(text = clipr::read_clip())

# 2016-2017 season:
player_stats_2017 <- read.csv(text = clipr::read_clip())

# 2017-2018 season:
player_stats_2018 <- read.csv(text = clipr::read_clip())

#'###### -------------**Binding**---------------------- ######

# Bind all the player statistic tables
all_nhl_player_stats <- bind_rows(player_stats_2008, player_stats_2009,
                                  player_stats_2010, player_stats_2011,
                                  player_stats_2012, player_stats_2014,
                                  player_stats_2015, player_stats_2016,
                                  player_stats_2017, player_stats_2018)

# Writing full player statistic table to csv
write_csv(all_nhl_player_stats, "all-player-stats-2007-2018-corsica.csv")


                        