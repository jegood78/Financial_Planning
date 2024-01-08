# load libraries
library(rvest)
library(magrittr)
library(lubridate)
library(tidyverse)

rClass <- "Officer"
#rClass <- "Enlisted"

#get the appropriate pay chart from "https://www.federalpay.org/military"
#Enlisted pay is on Table 1
#Officer pay is on Table 2

if (rClass == "Enlisted") {
  tn = 1  
} else if (rClass == "Officer") {
  tn = 2
} else {
  tn = "ERROR"
}

url <- "https://www.federalpay.org/military"

page <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

rt <- page %>% 
  html_nodes("table") %>%
  .[[tn]] %>%
  html_table(fill=T)

rt

#simplify column names
colnames(rt)[-1] <- gsub("-","",gsub(c("Ranks"),"",colnames(rt)[-1]))

colnames(rt)[1] <- gsub('Years of Experience','YOE',colnames(rt)[1])

# if rClass is "Officer" we need to separate the prior enlisted pays from O1, O2, and O3
# and convert pay values from character to number
if (rClass=="Officer") {
  rt1 <- separate(rt,"O1",c("O1","O1E"),"[*]")
  rt1 <- separate(rt1,"O2",c("O2","O2E"),"[*]")
  rt1 <- separate(rt1,"O3",c("O3","O3E"),"[*]")
  
  rt1$O1 <- parse_number(rt1$O1,na=c("","n/a"))
  rt1$O1E <- parse_number(rt1$O1E,na=c("","n/a"))
  rt1$O2 <- parse_number(rt1$O2,na=c("","n/a"))
  rt1$O2E <- parse_number(rt1$O2E,na=c("","n/a"))
  rt1$O3 <- parse_number(rt1$O3,na=c("","n/a"))
  rt1$O3E <- parse_number(rt1$O3E,na=c("","n/a"))
  rt1$O4 <- parse_number(rt1$O4,na=c("","n/a"))
  rt1$O5 <- parse_number(rt1$O5,na=c("","n/a"))
  rt1$O6 <- parse_number(rt1$O6,na=c("","n/a"))
  rt1$O7 <- parse_number(rt1$O7,na=c("","n/a"))
  rt1$O8 <- parse_number(rt1$O8,na=c("","n/a"))
  rt1$O9 <- parse_number(rt1$O9,na=c("","n/a"))
  rt1$O10 <- parse_number(rt1$O10,na=c("","n/a"))
  
}

rt1

write.csv(rt1, file = "/Users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv",row.names = FALSE)
