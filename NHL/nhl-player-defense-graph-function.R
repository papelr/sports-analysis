#' ---------
#' Title: NHL Stat Graphs
#' Subtitle: Focusing on several statistics, for @PredsTalk
#' Author: Robert Papel
#' ---------

#'###### ---------**Libraries**------- ######

library(tidyverse)

#'###### ---------**Data / Plot Function**------- ######

specific_stats <- read.csv("player-data-2008-2018.txt")

specific_stats <- 
  specific_stats %>% 
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

specific_stats <- 
  specific_stats %>% 
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
                         "17-18" = "2018"))

specific_stats$Team <- gsub("\\.", "", specific_stats$Team)
specific_stats$Player <- gsub("\\.", "", specific_stats$Player)

specific_stats <- specific_stats[specific_stats$Player != "Player", ]

nhl_hex_codes <- 
  c("ANA" = "#FC4C02", "ARI" = "#8C2633", "ATL" = "#000001",
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

specific_stats$Season <- as.Date(as.character(specific_stats$Season), 
                                   format = "%Y")
specific_stats$Season <- year(specific_stats$Season)
class(specific_stats$Season)

cf_plot <- specific_stats %>% 
  filter(Season != "2013") %>%
  mutate(DiscreteSeason = as.factor(Season)) %>% 
  group_by(Player) %>% 
  filter(Player == player) %>% 
  ggplot() +
  geom_line(aes_string("DiscreteSeason", statistic, fill = "Player")) +
  theme_bw() +
  coord_flip() +
  labs(
    title = player,
    subtitle = paste0(statistic, ", regular season"),
    x = "Season",
    y = statistic,
    caption = "https://www.corsicahockey.com, by R. Papel")

