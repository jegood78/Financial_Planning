#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load libraries
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(writexl)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# banking files
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#checking
checking_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_checking_raw.csv")

#create a key
checking_raw <- checking_raw %>%
  mutate(key = paste0(Description,"_",Amount,"_",Date))

#format dates
checking_raw$Date <- mdy(checking_raw$Date)

#create month column
checking_raw <- checking_raw %>%
  mutate(month = format(ymd(Date), "%Y-%m"))

#remove savings to prevent duplicate entries
checking_raw <- checking_raw %>%
  filter(!`Transaction Type` == "savings")

#remove USAA Credit Card Payments
checking_raw <- checking_raw %>%
  filter(!`Original Description` == "USAA CREDIT CARD PAYMENT")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#savings
savings_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_savings_raw.csv")

#create a key
savings_raw <- savings_raw %>%
  mutate(key = paste0(Description,"_",Amount,"_",Date))

#format dates
savings_raw$Date <- mdy(savings_raw$Date)

#create month column
savings_raw <- savings_raw %>%
  mutate(month = format(ymd(Date), "%Y-%m"))

#remove USAA Credit Card Payments
savings_raw <- savings_raw %>%
  filter(!`Original Description` == "USAA CREDIT CARD PAYMENT")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#visa
visa_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_visa_raw.csv")

#create a key
visa_raw <- visa_raw %>%
  mutate(key = paste0(Description,"_",Amount,"_",Date))

#format dates
visa_raw$Date <- mdy(visa_raw$Date)

#create month column
visa_raw <- visa_raw %>%
  mutate(month = format(ymd(Date), "%Y-%m"))

#remove USAA Credit Card Payments
visa_raw <- visa_raw %>%
  filter(!grepl("USAA CREDIT CARD PAYMENT",`Original Description`))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#combine them
bank <- rbind(checking_raw,
      savings_raw,
      visa_raw)

#check duplicates by key
bank %>% group_by(key) %>% summarise(count = n()) %>% arrange(desc(count))

#get rid of spaces in names
names(bank) <- sub(" ","_",names(bank))

#change all names to lower
names(bank) <- tolower(names(bank))

# do the same to category
#unique(bank$category)
bank$category <- gsub("& ","",bank$category)
bank$category <- gsub(" ","_",bank$category)
bank$category <- gsub("/","_",bank$category)
bank$category <- tolower(bank$category)

#export the new historicals
write_csv(bank, "/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_formatted.csv")

#group by month and transaction typ
bank %>% group_by(month, transaction_type) %>% summarise(monthly_amount = sum(amount))
