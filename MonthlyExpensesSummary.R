#libraries
library(tidyverse)
library(lubridate)
library(readxl)

#read in files
bank <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/Expenses.xlsx", 
                   sheet = "Transactions")

#change all of the names to lower case
names(bank) <- tolower(names(bank))

#change date from date-time to date
bank$date <- as.Date(bank$date)

#subset the data into pay, expenses, and investments
categories <- distinct(bank,category)
categories$category

income <- bank %>% filter(category %in% c("Jeff_Pay","Miscellaneous_Income"))

investments <- bank %>% filter(category %in% c("Investment_Account"))

expenses <- bank %>% filter(!category %in% c("Jeff_Pay","Miscellaneous_Income","Investment_Account"))

