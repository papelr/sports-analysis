#' ---------
#' Title: Player Statistics
#' Subtitle: Function to call a player & statistic, over the years
#' Author: Robert Papel
#' ---------

#'###### --------------------------------------------------- ###### 
#'###### ---------**Function for Stat & Player Plot**------- ######
#'###### --------------------------------------------------- ######

player_statistics <- function(player, statistic) {
  
  # Libraries
  library(tidyverse)
  library(readr)
  library(lubridate)
  
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
                           "7-8" = "2008",
                           "8-9" = "2009",
                           "9-10" = "2010",
                           "10-11" = "2011",
                           "11-12" = "2012",
                           "12-13" = "2013",
                           "13-14" = "2014",
                           "14-15" = "2015",
                           "15-16" = "2016",
                           "16-17" = "2017",
                           "17-18" = "2018")) %>% 
    mutate(Player = recode(Player,
                           "P K..subban" = "PK Subban"))
  
  # Getting rid of all periods in Team & Player
  all_player_stats$Team <- gsub("\\.", "", all_player_stats$Team)
  all_player_stats$Player <- gsub("\\.", "", all_player_stats$Player)
  
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
      "WSH" = "#041E42", "WPG" = "#53565A", "COL/ OTT" = "#C8102E",
      "MTL" = "#A6192E", "ARI/ OTT" = "#C8102E", "OTT/ NSH" = "#FFB81C",
      "NSH/ CBJ" = "#041E42")
  
  # Function to set YEAR scale to number of seasons played by pitcher
  f <- function(k) {
    step <- k
    function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
  }
  
  
  # Statistic scale function with a switch, *need to build*
  
  # Converting Season column to an actual year date
  all_player_stats$Season <- as.Date(as.character(all_player_stats$Season), 
                                     format = "%Y")
  all_player_stats$Season <- year(all_player_stats$Season)
  class(all_player_stats$Season)
  
  # ggplot of player and chosen statistic
  pp <- all_player_stats %>% 
    filter(Season != "2013") %>%
    mutate(DiscreteSeason = as.factor(Season)) %>% 
    group_by(Player) %>% 
    filter(Player == player) %>% 
    ggplot() +
    geom_col(aes_string("DiscreteSeason", statistic, fill = "Team"),
             color = "#000000", width = .5) +
    scale_fill_manual(values = nhl_hex_codes) +
    scale_x_discrete(breaks = waiver()) + # Uses the function to set YEAR breaks
    # scale_y_continuous(breaks = ticks(statistic)) +
    theme_bw() +
    coord_flip() +
    labs(
      title = player,
      subtitle = paste0(statistic, ", regular season"),
      x = "Season",
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

# Employing the function
player_statistics("Nikita Kucherov", "GP")



# Future thoughts: 
# Could do a nested function where you have the player data, goalie data, WAR data, and team data, by just adding 3 more data sets. I mean to make a function within the function, and add a third argument to the main function. I.e., have the function: function(player, statistic, dataset), where player can be replaced by team