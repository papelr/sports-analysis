#' ---------
#' Title: How whip Changes with Age
#' Subtitle: MLB - whip Stat
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(rvest)

#'###### -------------**Scrape**---------------------- ######

# Links for pitching stats, includes WHIP statistic
whip_2018a <- "http://www.espn.com/mlb/stats/pitching/_/order/false"
whip_2018b <- "http://www.espn.com/mlb/stats/pitching/_/count/41/qualified/true/order/false"
whip_2018c <- "http://www.espn.com/mlb/stats/pitching/_/count/81/qualified/true/order/false"

whip_2017a <- "http://www.espn.com/mlb/stats/pitching/_/year/2017/order/false"
whip_2017b <- "http://www.espn.com/mlb/stats/pitching/_/year/2017/count/41/qualified/true/order/false")

whip_2016 <- "http://www.espn.com/mlb/stats/pitching/_/year/2016/order/false"
whip_2015 <- "http://www.espn.com/mlb/stats/pitching/_/year/2015/order/false"
whip_2014 <- "http://www.espn.com/mlb/stats/pitching/_/year/2014/order/false"
whip_2013 <- "http://www.espn.com/mlb/stats/pitching/_/year/2013/order/false"
whip_2012 <- "http://www.espn.com/mlb/stats/pitching/_/year/2012/order/false"
whip_2011 <- "http://www.espn.com/mlb/stats/pitching/_/year/2011/order/false"
whip_2010 <- "http://www.espn.com/mlb/stats/pitching/_/year/2010/order/false"
whip_2009 <- "http://www.espn.com/mlb/stats/pitching/_/year/2009/order/false"
whip_2008 <- "http://www.espn.com/mlb/stats/pitching/_/year/2008/order/false"
whip_2007 <- "http://www.espn.com/mlb/stats/pitching/_/year/2007/order/false"
whip_2006 <- "http://www.espn.com/mlb/stats/pitching/_/year/2006/order/false"
whip_2005 <- "http://www.espn.com/mlb/stats/pitching/_/year/2005/order/false"
               
# Scraping 2018 tables - there are multiple pages to the list
a2018 <- whip_2018a %>%
  read_html %>%
  html_node("#my-players-table > div > div.mod-content > table") %>%
  html_table(header = T)
  
b2018 <- whip_2018b %>%
  read_html %>%
  html_node("#my-players-table > div > div.mod-content > table") %>%
  html_table(header = T)
  
c2018 <- whip_2018c %>%
  read_html %>%
  html_node("#my-players-table > div > div.mod-content > table") %>%
  html_table(header = T)

whip_2018 <- rbind(a2018, b2018, c2018)

# Adjusting rows
names(whip_2018) <- as.matrix(whip_2018[1, ])
whip_2018 <- whip_2018[-1, ]
whip_2018 <- whip_2018[c(-11,-22,-33,-44,-55,-66,-77,-88), ]
whip_2018$"YEAR" <- 2018 
whip_2018[] <- lapply(whip_2018, function(x) type.convert(as.character(x)))

whip_2018 <- whip_2018 %>% 
  select(YEAR, PLAYER, TEAM, GP, IP, SO, W, L, WAR, WHIP, ERA)




#'###### -------------**Detritus**---------------------- ######

"http://www.espn.com/mlb/stats/pitching/_/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2017/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2016/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2015/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2014/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2013/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2012/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2011/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2010/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2009/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2008/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2007/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2006/order/false",
"http://www.espn.com/mlb/stats/pitching/_/year/2005/order/false"