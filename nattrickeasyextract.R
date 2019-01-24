
library(rvest)
library(formattable)
library(tidyverse)

url <- 'https://www.naturalstattrick.com/playerteams.php?fromseason=20182019&thruseason=20182019&stype=2&sit=5v5&score=all&stdoi=oi&rate=n&team=NSH&pos=S&loc=B&toi=0&gpfilt=none&fd=&td=&tgp=410&lines=single'

test <- url %>% 
  read_html() %>% 
  # html_nodes('td') %>% 
  html_table(fill = T, header = T) 

coerced <- as.data.frame(test)

coerced %>% 
  select(Player, TOI, CF.) %>% 
  formattable()

table1 <- coerced %>% 
  ggplot() +
  geom_col(aes(Player, CF.)) +
  coord_flip()

print(table1)



# .sorting_desc , .sorting , td