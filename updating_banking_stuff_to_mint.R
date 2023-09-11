
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load libraries
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load files
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#read in mint transactions
bank <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_transactions.csv") %>%
  select(!c(Labels,Notes))

#read in mint categories
cats <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_categories.xlsx",
                   sheet = "reference")

#get rid of spaces in names
names(bank) <- sub(" ","_",names(bank))

#change all names to lower
names(bank) <- tolower(names(bank))

# do the same to category and account_name
unique(bank$category)
bank$category <- gsub("& ","",bank$category)
bank$category <- gsub(" ","_",bank$category)
bank$category <- tolower(bank$category)

unique(bank$account_name)
bank$account_name <- gsub(" ","_",bank$account_name)
bank$account_name <- tolower(bank$account_name)
bank$account_name <- gsub("_-_uniformed_services","",bank$account_name)


#change dates to date type
bank$date <- as.Date(mdy(bank$date))

#add HOF category
bank <- bank %>%
  left_join(cats, by = "category")





