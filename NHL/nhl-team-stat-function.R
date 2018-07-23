#' ---------
#' Title: Team Statistics
#' Subtitle: Function to call a player & team, over the years
#' Author: Robert Papel
#' ---------

#'###### --------------------------------------------------- ###### 
#'###### ---------**Function for Team Statistics Plot**----- ######
#'###### --------------------------------------------------- ######


team_statistics <- function(team, statistic) {
  
  # Libraries
  library(tidyverse)
  library(readr)
  library(lubridate)
  
  # Set working directory
 setwd("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/data")
  
  # Player statistics 
  all_team_stats <- read.csv("team-data-2008-2018.txt")
  
  # Adjusting columns names
  all_team_stats <- 
    all_team_stats %>% 
    rename(Corsi_plus_minus = C...,
           CF_Perc = CF.,
           F_plus_minus = F...,
           FF_Perc = FF.,
           S_plus_minus = S...,
           SF_Perc = SF.,
           G_plus_minus = G...,
           GF_Perc = GF.,
           xG_plus_minus = xG...,
           xGF_Perc = xGF.,
           P_plus_minus = P...,
           Sh_Perc = Sh.,
           Sv_Perc = Sv.,
           FSh_Perc = FSh.,
           FSv_Perc = FSv.,
           xFSh_Perc = xFSh.,
           xFSv_Perc = xFSv.,
           dFSh_Perc = dFSh.,
           dFSv_Perc = dFSv.)
  
  # Changing Season-year format
  all_team_stats <- 
    all_team_stats %>% 
    mutate(Season = recode(Season, 
                           "07-08" = "2008",
                           "08-09" = "2009",
                           "09-10" = "2010",
                           "10-11" = "2011",
                           "11-12" = "2012",
                           "12-13" = "2013",
                           "13-14" = "2014",
                           "14-15" = "2015",
                           "15-16" = "2016",
                           "16-17" = "2017",
                           "17-18" = "2018"))
  
  # Getting rid of all periods in Team & Player
  all_team_stats$Team <- gsub("\\.", "", all_team_stats$Team)

  # Getting rid of repeated header lines in table
  all_team_stats <- all_team_stats[all_team_stats$Team != "Team", ]

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
      "WSH" = "#041E42", "WPG" = "#53565A", "TBL" = "#00205B",
      "SJS" = "#006272", "MTL" = "#A6192E", "LAK" = "#000000",
      "NJD" = "#C8102E", "PHI" = "#FA4616", "WAS" = "#041E42")
  
  # Statistic scale function with a switch, *need to build*
  
  # Converting Season column to an actual year date
  all_team_stats$Season <- as.Date(as.character(all_team_stats$Season), 
                                     format = "%Y")
  all_team_stats$Season <- year(all_team_stats$Season)

  # Renaming from team abbreviations to full names
  # team_names <- 
  #   c("ANA" = "Anaheim Ducks",
  #     "ARI" = "Arizona Coyotes", "BUF" = "Buffalo Sabres",
  #     "CGY" = "Calgary Flames", "CAR" = "Carolina Hurricanes",
  #     "CHI" = "Chicago Blackhawks", "COL" = "Colorado Avalanche",
  #     "CBJ" = "Columbus Blue Jackets", "DAL" = "Dallas Stars",
  #     "DET" = "Detroit Red Wings", "EDM" = "Edmonton Oilers",
  #     "FLA" = "Florida Panthers", "LAK" = "Los Angeles Kings",
  #     "MIN" = "Minnesota Wild", "MON" = "Montreal Canadiens", 
  #     "NJD" = "New Jersey Devils", "NSH" = "Nashville Predators",
  #     "NYI" = "New York Islanders",
  #     "NYR" = "New York Rangers", "OTT" = "Ottowa Senators",
  #     "PHI" = "Philadelphia Flyers", "PIT" = "Pittsburgh Penguins",
  #     "STL" = "St. Louis Blues", "SJS" = "San Jose Sharks",
  #     "TBL" = "Tampa Bay Lightning", "TOR" = "Tornona Maple Leafs",
  #     "VAN" = "Vancouver Canucks", "VGK" = "Vegas Golden Knights",
  #     "WSH" = "Washington Capitals", "WPG" = "Winnipeg Jets")
  
  # ggplot of player and chosen statistic
  w <- all_team_stats %>% 
    filter(Season != "2013") %>%
    mutate(DiscreteSeason = as.factor(Season)) %>% 
    group_by(Team) %>% 
    filter(Team == team) %>% 
    ggplot() +
    geom_col(aes_string("DiscreteSeason", statistic, fill = "Team"),
             color = "#000000", width = .5, show.legend = F) +
    scale_fill_manual(values = nhl_hex_codes) +
    scale_x_discrete(breaks = waiver()) + # Uses the function to set YEAR breaks
    # scale_y_continuous(breaks = ticks(statistic)) +
    theme_bw() +
    coord_flip() +
    labs(
      title = team,
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
    )
  
  # ggsave of pitcher's stat plot
  ggsave(w, file = paste0
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/NHL/plots-teams/", team, statistic, ".png"), 
         device = "png",
         width = 10,
         height = 7)
  
  return(w)
}

# Employing the function
team_statistics("WAS", "xGF")