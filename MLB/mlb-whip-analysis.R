#' ---------
#' Title: How WHIP Changes with Age
#' Subtitle: MLB - WHIP Stat
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(rvest)
library(ggrepel)

#'###### -------------**Scrape Function**---------------------- ######

# A function that creates a "year" parameter within the ESPN links
espn_pitching_stats <- function(season) {
  
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

# Uses the function to pull in 2000-201_ seasons
whip_table <- lapply(2000:2017, espn_pitching_stats)
final_table <- do.call(rbind, whip_table)  


#'###### -------------**Tidying Table**---------------------- ######

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


#'###### -------------**Visualizations**---------------------- ######

# Narrowing down data set to players, WHIP, and year
final_table %>% 
  select(PLAYER, YEAR, WHIP, TEAM) %>% 
  group_by(PLAYER) %>% 
  filter(n_distinct(YEAR) > 1)  # If a player has played certain # of years

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
                     "MON" = "#AB0003")

# Function to set YEAR scale to number of seasons played by pitcher
f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)       
}

# Picking a player and looking at their WHIP, or whatever statistic
final_table %>% 
  select(PLAYER, WHIP, YEAR, TEAM) %>% 
  group_by(PLAYER) %>% 
  filter(PLAYER == "R.A. Dickey") %>% 
  ggplot() +
  geom_col(aes(YEAR, WHIP, fill = TEAM), width = .5) +
  # scale_x_reverse() +
  scale_fill_manual(values = mlb_team_colors) +
  scale_x_continuous(breaks = f(1)) +  # Uses the function to set YEAR breaks
  scale_y_continuous(breaks = f(0.1)) +
  theme_bw() +
  coord_flip() +
  labs(
    title = "WHIP Statistic: R.A. Dickey",
    subtitle = "WHIP over seasons played",
    x = "Year",
    y = "WHIP Stat",
    caption = "Data from espn.com, Plot by R. Papel") +
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


#'###### -------------**WHIP Modeling**---------------------- ######

# Linear model?
final_table %>% 
  select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, H, R, BB, ER, WAR, WHIP) %>% 
  group_by(PLAYER) %>% 
  filter(YEAR == "2017") %>% 
  lm(WHIP ~  IP + H + BB, .) %>%  # The WHIP stat is made up of these 3 IVs
  summary()
  
# Visualize linear model, can change y-axis variable 
final_table %>% 
  select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, H, R, BB, ER, WAR, WHIP) %>% 
  group_by(PLAYER) %>% 
  filter(YEAR == "2017",
         TEAM != "TEX/LAD", # Removing trades, for graphic clean-up
         TEAM != "CHW/CHC",
         TEAM != "NYY/OAK",
         TEAM != "SEA/STL",
         TEAM != "BAL/PHI",
         TEAM != "DET/HOU") %>% # This is Verlander, so...
  ggplot(aes(WHIP, IP)) +
  geom_point(aes(color = TEAM), size = 3, shape = 18, show.legend = F) +
  scale_color_manual(values = mlb_team_colors) +
  geom_label_repel(aes(label = TEAM), size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 0.3, color = "black",
              weight = 0.5)
  
  
#'###### -------------**Function for Stat/Player**---------- ######

# A function that takes the scraped dataset and returns a plot of PLAYER
# and whatever statistic is called, filled by the PLAYER's team

# Function:
baseball_stats <- function(player, statistic) {
  
  # Libraries
  library(tidyverse)
  library(rvest)
  library(ggrepel)
  
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
                       "MON" = "#AB0003")
  
  # Function to set YEAR scale to number of seasons played by pitcher
  f <- function(k) {
    step <- k
    function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
  }
  
  # ggplot of player and chosen statistic
  p <- final_table %>% 
    group_by(PLAYER) %>% 
    filter(PLAYER == player) %>% 
    ggplot() +
    geom_col(aes_string("YEAR", statistic, fill = "TEAM"), width = .5) +
    scale_fill_manual(values = mlb_team_colors) +
    scale_x_continuous(breaks = f(1)) +  # Uses the function to set YEAR breaks
    scale_y_continuous(breaks = f(0.5)) +
    theme_bw() +
    coord_flip() +
    labs(
      title = player,
      subtitle = statistic,
      x = "Year",
      y = statistic,
      caption = "Data from espn.com, Plot by R. Papel") +
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
  
  return(p)
 
}

# Testing function, good for smaller numbered stats (WHIP, ERA, WAR)
baseball_stats("R.A. Dickey", "WHIP")




#'###### -------------**An All-Encompassing Function**---------- ######

# Testing to see if I can put the scrape function in the plot function,
# which includes the table formatting before plotting... we'll see