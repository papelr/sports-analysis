#' ---------
#' Title: Player Statistics
#' Subtitle: Function to call a player & statistic, over the years
#' Author: Robert Papel
#' ---------

#'###### --------------------------------------------------- ###### 
#'###### ---------**Function for Stat & Player Plot**------- ######
#'###### --------------------------------------------------- ######

nhl_statistics <- function(player, statistic) {
  
  # Libraries
  library(tidyverse)
  library(readr)
  
  # Set working directory
  setwd("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/player-data")
  
  # Player statistics 
  all_player_stats <- read.csv("player-data-2008-2018.txt")
  
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
  
  # Changing Season-year format
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
                           "17-18" = "2017-2018")) %>% 
    mutate(Player = recode(Player,
                           "P K..subban" = "PK Subban"))
  
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
  
  # Function to set YEAR scale to number of seasons played by pitcher
  f <- function(k) {
    step <- k
    function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
  }
  
  # Statistic scale function
  
  # ggplot of player and chosen statistic
  pp <- all_player_stats %>% 
    group_by(Player) %>% 
    filter(Player == player) %>% 
    ggplot() +
    geom_col(aes_string("Player", statistic, fill = "Team"),
             color = "#000000", width = .5) +
    # scale_fill_manual(values = nhl_hex_codes) +
    scale_x_continuous(breaks = f(1)) +  # Uses the function to set YEAR breaks
    # scale_y_continuous(breaks = ticks(statistic)) +
    theme_bw() +
    coord_flip() +
    labs(
      title = player,
      subtitle = statistic,
      x = "Year",
      y = statistic,
      caption = "https://www.corsicahockey.com, by R. Papel") +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.caption = element_text(face = "italic"),
      plot.subtitle = element_text(face = "italic"),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      legend.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(color = "grey70"), 
      axis.title.y = element_text(size = 11, face = "bold", color = "black"),
      axis.title.x = element_text(size = 11, face = "bold", color = "black")
    ) +
    guides(fill = guide_legend(title = "Team"))
  
  # ggsave of pitcher's stat plot
  ggsave(pp, file = paste0
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-players/", player, statistic, ".png"), 
         device = "png",
         width = 10,
         height = 7)
  
  return(pp)
}

nhl_statistics("PK Subban", "GP")

