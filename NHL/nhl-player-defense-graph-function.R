#' ---------
#' Title: NHL Stat Graphs
#' Subtitle: Focusing on several statistics, for @PredsTalk
#' Author: Robert Papel
#' ---------

#'###### ---------**Libraries**------- ######

library(tidyverse)
library(lubridate)

#'###### ---------**Data**------- ######

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


#'###### ---------**CF% Plot**------- ######

player_colors <- c("Ryan Ellis" = "#FFB81C", "Dustin Byfuglien" = "#53565A",
                   "Ryan Suter" = "#154734", "Alex Pietrangelo" = "#003087",
                   "Erik Johnson" = "#6F263D", "Brent Burns" = "#006272",
                   "Cam Fowler" = "#FC4C02", "Mark Giordano" = "#C8102E",
                   "Alex Edler" = "#00843D", "John Klingberg" = "#006341",
                   "Oliver Ekman-larsson" = "#8C2633", 
                   "Drew Doughty" = "#000000", 
                   "Oscar Klefbom" = "#041E42", "Nate Schmidt" = "#B9975B",
                   "Duncan Keith" = "#C8102E")

cf_plot <- specific_stats %>% 
  filter(Player %in% c("Ryan Ellis", "Dustin Byfuglien", "Ryan Suter", 
                       "Drew Doughty", "Alex Pietrangelo", "Erik Johnson",
                       "Brent Burns", "Cam Fowler", "Mark Giordano", 
                       "Alex Edler", "John Klingberg", "Oliver Ekman-larsson",                        "Oscar Klefbom"," Nate Schmidt", "Duncan Keith")) %>% 
  filter(Season >= "2014") %>%
  mutate(DiscreteSeason = as.factor(Season)) %>%
  ggplot(aes(Season, CF_Perc, color = Player, group = Player)) +
  scale_color_manual(values = player_colors) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(
    title = "Ryan Ellis vs. Other No. 1 D-Men",
    subtitle = "Corsi For %",
    x = "Season",
    y = "CF %",
    caption = "https://www.corsicahockey.com, by R. Papel") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(face = "italic"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey70"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x = element_text(size = 11, face = "bold", color = "black")
  ) 

ggsave(cf_plot, file = 
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-players/RyanEllisCF_Perc.png"), 
       device = "png",
       width = 10,
       height = 7)


#'###### ---------**xGF_Perc Plot**------- ######

xGF_plot <- specific_stats %>% 
  filter(Player %in% c("Ryan Ellis", "Dustin Byfuglien", "Ryan Suter", 
                       "Drew Doughty", "Alex Pietrangelo", "Erik Johnson",
                       "Brent Burns", "Cam Fowler", "Mark Giordano", 
                       "Alex Edler", "John Klingberg", "Oliver Ekman-larsson",                        "Oscar Klefbom"," Nate Schmidt", "Duncan Keith")) %>% 
  filter(Season >= "2014") %>%
  mutate(DiscreteSeason = as.factor(Season)) %>%
  ggplot(aes(Season, xGF_Perc, color = Player, group = Player)) +
  scale_color_manual(values = player_colors) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(
    title = "Ryan Ellis vs. Other No. 1 D-Men",
    subtitle = "xGF %",
    x = "Season",
    y = "xGF %",
    caption = "https://www.corsicahockey.com, by R. Papel") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(face = "italic"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey70"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x = element_text(size = 11, face = "bold", color = "black")
  ) 

ggsave(xGF_plot, file = 
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-players/RyanEllis_xGF_Perc.png"), 
       device = "png",
       width = 10,
       height = 7)


#'###### ---------**CF_Qual_of_Team Plot**------- ######

CF_Qual_of_Team_plot <- specific_stats %>% 
  filter(Player %in% c("Ryan Ellis", "Dustin Byfuglien", "Ryan Suter", 
                       "Drew Doughty", "Alex Pietrangelo", "Erik Johnson",
                       "Brent Burns", "Cam Fowler", "Mark Giordano", 
                       "Alex Edler", "John Klingberg", "Oliver Ekman-larsson",                        "Oscar Klefbom"," Nate Schmidt", "Duncan Keith")) %>% 
  filter(Season >= "2014") %>%
  mutate(DiscreteSeason = as.factor(Season)) %>%
  ggplot(aes(Season, CF_Qual_of_Team, color = Player, group = Player)) +
  scale_color_manual(values = player_colors) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(
    title = "Ryan Ellis vs. Other No. 1 D-Men",
    subtitle = "CF_Qual_of_Team",
    x = "Season",
    y = "CF_Qual_of_Team",
    caption = "https://www.corsicahockey.com, by R. Papel") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(face = "italic"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey70"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x = element_text(size = 11, face = "bold", color = "black")
  ) 

ggsave(CF_Qual_of_Team_plot, file = 
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-players/RyanEllis_CF_Qual_of_Team.png"), 
       device = "png",
       width = 10,
       height = 7)


#'###### ---------**ATOI Plot**------- ######

specific_stats$TOI <- as.numeric(specific_stats$TOI)
specific_stats$GP <- as.numeric(specific_stats$GP)


# math not quite working out
ATOI_plot <- specific_stats %>% 
  filter(Player %in% c("Ryan Ellis", "Dustin Byfuglien", "Ryan Suter", 
                       "Drew Doughty", "Alex Pietrangelo", "Erik Johnson",
                       "Brent Burns", "Cam Fowler", "Mark Giordano", 
                       "Alex Edler", "John Klingberg", "Oliver Ekman-larsson",                        "Oscar Klefbom"," Nate Schmidt", "Duncan Keith")) %>% 
  filter(Season >= "2014") %>%
  mutate(DiscreteSeason = as.factor(Season)) %>%
  mutate(ATOI = (TOI / GP)) %>% 
  ggplot(aes(Season, ATOI, color = Player, group = Player)) +
  scale_color_manual(values = player_colors) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(
    title = "Ryan Ellis vs. Other No. 1 D-Men",
    subtitle = "Average Time on Ice",
    x = "Season",
    y = "ATOI",
    caption = "https://www.corsicahockey.com, by R. Papel") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(face = "italic", size = 8),
    plot.subtitle = element_text(face = "italic"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey70"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x = element_text(size = 11, face = "bold", color = "black")
  ) 

ggsave(ATOI_plot, file = 
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-players/RyanEllis_ATOI.png"), 
       device = "png",
       width = 10,
       height = 7)
