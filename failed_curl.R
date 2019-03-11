library(curl)
library(tidyverse)

con <- curl('https://electionresults.dcboe.org/Downloads/Reports/November_6_2018_General_Election_Certified_Results.csv')

gen18 <- readLines(con)

close(con)

gen18[2] %>% 
  as.character() %>% 
  read_csv(col_names = FALSE)
