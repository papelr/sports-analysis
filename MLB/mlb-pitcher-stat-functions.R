#' ---------
#' Title: MLB Pitcher Statistics
#' Subtitle: Functions for Scraping and Visualizing
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### ---------------------------------------------------- ######
#'###### -------------**Scrape Function**-------------------- ######
#'###### ---------------------------------------------------- ######

# Run this section first, before any visualizations!

# A function that creates a "year" parameter within the ESPN links
espn_pitching_stats <- function(season) {
  
  # Libraries
  library(tidyverse)
  library(rvest)
  
  # Creating the URL vectors, without the year
  whip_a <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/order/false")
  whip_b <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/count/41/qualified/true/order/false")
  whip_c <- paste0("http://www.espn.com/mlb/stats/pitching/_/year/", season, 
                   "/count/81/order/false")
  
  # Building the data tables, ignores the 3rd link if only two pages in table
  a_data <- whip_a %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  b_data <- whip_b %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  c_data <- whip_c %>%
    read_html %>%
    html_node("#my-players-table > div > div.mod-content > table") %>%
    html_table(header = T)
  
  # Binding all the tables together
  pitching_tables <- rbind(a_data, b_data, c_data)   
  pitching_tables$Year <- season
  
  return(pitching_tables)
}

# Uses the function to pull in 2000-2018 seasons
whip_table <- lapply(2000:2018, espn_pitching_stats)
final_table <- do.call(rbind, whip_table)  



#'###### --------------------------------------------------- ###### 
#'###### ---------**Function for Stat & Player Plot**------- ######
#'###### --------------------------------------------------- ######

# A function that takes the scraped dataset and returns a plot of PLAYER
# and whatever statistic is called, filled by the PLAYER's team

# Function:
baseball_stats <- function(player, statistic) {
  
  # Libraries
  library(tidyverse)
  
  # Adjusting column headers
  names(final_table) <- as.matrix(final_table[1, ])
  final_table <- final_table[-1, ]
  
  # Getting rid of inner headers - the RK PLAYER TEAM, etc.
  final_table <- final_table[final_table$RK != "RK", ]
  final_table[] <- lapply(
    final_table, function(x) type.convert(as.character(x), as.is = TRUE))
  
  # Selecting specific variables
  final_table$YEAR <- final_table$`2000`
  final_table <- final_table %>% 
    select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, H, R, BB, ER, WAR, WHIP, ERA)
  
  # Assign TEAM color hex codes, probably missing an acronym somewhere
  mlb_team_colors <- c("ARI" = "#A71930", "ATL" = "#CE1141", "BAL" = "#DF4601",
                       "BOS" = "#BD3039", "CHC" = "#CC3433", "CWS" = "#000000",
                       "CIN" = "#C6011F", "CLE" = "#E31937", "COL" = "#333366",
                       "DET" = "#0C2C56", "HOU" = "#EB6E1F", "KC" = "#004687",
                       "LAA" = "#BA0021", "LA" = "#EF3E42", "LAD" = "#EF3E42",
                       "MIA" = "#FF6600", "FLA" = "#FF6600", "MIL" = "#B6922E",
                       "MIN" = "#002B5C", "NYM" = "#FF5910", "NYY" = "#003087",
                       "OAK" = "#003831", "PHI" = "#284898", "PIT" = "#FDB827",
                       "SD" = "#002D62", "SF" = "#FD5A1E", "SEA" = "#005C5C",
                       "STL" = "#C41E3A", "TB" = "#8FBCE6", "TEX" = "#C0111F",
                       "TOR" = "#134A8E", "WAS" = "#AB0003", "WSH" = "#AB0003",
                       "MON" = "#AB0003", "CHW" = "#000000", 
                       "DET/HOU" = "#EB6E1F", "BOS/OAK" = "#BD3039",
                       "DET/TOR" = "#134A8E", "DET/TB" = "#0C2C56")
  
  # Function to set YEAR scale to number of seasons played by pitcher
  f <- function(k) {
    step <- k
    function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
  }
  
  # Function to set scale, need to update this to a switch
  ticks <- function(stat) {
    if (stat == "WHIP") {
      step <- 0.1
    } else if (stat == "ERA") {
      step <- 0.25
    } else if (stat == "IP") {
      step <- 12
    } else if (stat == "H") {
      step <- 8
    } else if (stat == "R") {
      step <- 5
    } else if (stat == "ER") {
      step <- 4
    } else if (stat == "BB") {
      step <- 4
    } else if (stat == "SO") {
      step <- 12
    } else if (stat == "W") {
      step <- 1
    } else if (stat == "L") {
      step <- 1
    } else if (stat == "WAR") {
      step <- 1
    }
    return(f(step))
  }
  
  # ggplot of player and chosen statistic
  p <- final_table %>% 
    group_by(PLAYER) %>% 
    filter(PLAYER == player) %>% 
    ggplot() +
    geom_col(aes_string("YEAR", statistic, fill = "TEAM"),
                        color = "#000000", width = .5) +
    scale_fill_manual(values = mlb_team_colors) +
    scale_x_continuous(breaks = f(1)) +  # Uses the function to set YEAR breaks
    scale_y_continuous(breaks = ticks(statistic)) +
    theme_bw() +
    coord_flip() +
    labs(
      title = player,
      subtitle = statistic,
      x = "Year",
      y = statistic,
      caption = "http://www.espn.com/mlb/stats/pitching, by R. Papel") +
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
  ggsave(p, file = paste0
         ("/Users/robertpapel/Documents/Personal_R_Stuff/sports-analysis/MLB/plots-pitchers/", player, statistic, ".png"), 
         device = "png",
         width = 10,
         height = 7)
  
  return(p)
}

# Calling the function
baseball_stats("Rick Porcello", "ERA")
