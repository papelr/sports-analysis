library(rvest)
library(dplyr)
library(tm)
library(stringi)
library(readr)
library(mailR)
library(rJava)

# Scraping betting lines
tomorrow <- Sys.Date()
tomorrow <- gsub("-", "", tomorrow, fixed=TRUE)

print(tomorrow)

url <- read_html(paste0('https://classic.sportsbookreview.com/betting-odds/nhl-hockey/totals/?date=', as.character(tomorrow)))

rot <- url %>%
  html_nodes('.eventLine-rotation .eventLine-book-value') %>%
  html_text()

teams <- url %>%
  html_nodes('.team-name a') %>%
  html_text()

total <- url %>%
  html_nodes('.adjust')%>%
  html_text()

opening <- url %>%
  html_nodes('.price') %>%
  html_text()

pinnacle <- url %>%
  html_nodes('.eventLine-book:nth-child(10) b') %>%
  html_text()

dimes <- url %>%
  html_nodes('.eventLine-book:nth-child(11) b') %>%
  html_text()

BookMaker <- url %>%
  html_nodes('.eventLine-book:nth-child(12) b') %>%
  html_text()

BETONLINE <- url %>%
  html_nodes('.eventLine-book:nth-child(13) b') %>%
  html_text()

BOVADA <- url %>%
  html_nodes('.eventLine-book:nth-child(14) b') %>%
  html_text()

HERITAGE <- url %>%
  html_nodes('.eventLine-book:nth-child(15) b') %>%
  html_text()

roster <- data.frame(
  ROT = rot, TEAM = teams, Total = total, OPENER = opening, PINNACLE = pinnacle,
  FiveDimes = dimes, BOVADA = BOVADA, BETONLINE = BETONLINE,
  HERITAGE = HERITAGE, BookMaker = BookMaker)

nhl_odds <- write_excel_csv(roster,'NHL_TOTALS_TODAY.csv')

# Setting email to send out odds daily
send.mail(from = "robertpapel@gmail.com",
          to = "papel_robert@bah.com",
          subject = "Test R automation",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 465, user.name = "robertpapel", 
                      passwd = "rp37205_", ssl = TRUE),
          authenticate = T,
          send = TRUE)


library(gmailr)

gmail_email <- mime() %>%
  to("rdp3d@virginia.edu") %>%
  from("robertpapel@gmail.com") %>%
  subject("Test R Email Woot") %>% 
  text_body("My First Email using R.") %>% 
  attach_file("NHL_TOTALS_TODAY.csv")

send_message(gmail_email)






