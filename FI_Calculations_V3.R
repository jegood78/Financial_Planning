# Things to do:
# Load libraries - Done
# Load files - Done
# Set user inputs - Done
# Define functions - Done
# Calculate pensions:
#   if I retired today
#   estimated retirement
# Calculate average expenses for last 12 months
# Calculate average income for last 12 months
# Calculate average investments for last 12 months
# Calculate current net worth
# Calculate FU numbers:
#   Naive
#   With retire today pension
#   With estimated retirement pension

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
#read in table of historical S&P 500 returns
sp_500 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/sp-500-historical-annual-returns.csv")

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

#read expenses file in 
bank <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/Expenses.xlsx",
                   sheet = "Transactions")

#load military pay data
pChart2023 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv")
pChart2022 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2022_Officer_Pay.csv")
pChart2021 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2021_Officer_Pay.csv")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Set user inputs
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#housing buffer for expenses since we don't currently pay for housing
housing_buffer <- 3000

#bah amount to add back into pay
bah <- 2640

#set annual safe withdrawal rate
annual_safe_withdrawal <- 0.03

#set TSP contributions
tsp <- 20500

#set pay entry base date
pebd <- as.Date(mdy("2-19-1999")) #pay entry based date

#set current rank
c_rank <- "O4" 

#set current date of rank
c_dor <- as.Date(mdy("9-1-2018"))

#estimated promotion date
est_promotion <- as.Date(mdy("9-1-2023")) 

#estimated retirement date
est_retire_date <- as.Date(mdy("09-1-2026"))

#create a 12 month list
rolling_12_months <- format(seq(today()-365, today() %m-% months(1), by = 'month'), "%Y-%m")

#current age
c_age <- 44

#estimated EOL
est_eol_age <- 90

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Define functions
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#function to calculate federal taxes based on 2023 married filing jointly tax brackets
calculate_net_income <- function(x){
  if (x <= 22000) {
    net_pay <- x*(1-0.10)
  } 
  else if (x > 22000 & x <= 89450) {
    net_pay <- 22000*(1-0.10) + 
      (22000)*(1-0.12) 
  } 
  else if (x > 89450 & x <= 190750) {
    net_pay <- 22000*(1-0.10) + 
      (89450-22000)*(1-0.12) + 
      (x-89450)*(1-0.22)
  } 
  else if (x > 190750 & x <= 364200) {
    net_pay <- 22000*(1-0.10) + 
      (89450-22000)*(1-0.12) + 
      (190750-89450)*(1-0.22) + 
      (x-190750)*(1-0.24)
  }
  else if (x > 364200 & x <= 462500) {
    net_pay <- 22000*(1-0.10) + 
      (89450-22000)*(1-0.12) + 
      (190750-89450)*(1-0.22) + 
      (364200-190750)*(1-0.24) +
      (x-364200) * (1-0.32)
  }
  else if (x > 462500 & x <= 693750) {
    net_pay <- 22000*(1-0.10) + 
      (89450-22000)*(1-0.12) + 
      (190750-89450)*(1-0.22) + 
      (364200-190750)*(1-0.24) +
      (462500-364200) * (1-0.32) +
      (x-462500) * (1-0.35)
  }
  else if (x > 693750) {
    net_pay <- 22000*(1-0.10) + 
      (89450-22000)*(1-0.12) + 
      (190750-89450)*(1-0.22) + 
      (364200-190750)*(1-0.24) +
      (462500-364200) * (1-0.32) +
      (693750-462500) * (1-0.35) +
      (x-693750) * (1-0.37)
  }
  return(round(net_pay))
}

#function to calculate years of experience (yoe)
calculate_yoe <- function(x) {
  if (x <= 2) {
    c_yoe = "2 or less"
  } else if (x > 40) {
    c_yoe = "Over 40"
  } else if (x > 38) {
    c_yoe = "Over 38"
  } else if (x > 36) {
    c_yoe = "Over 36"
  } else if (x > 34) {
    c_yoe = "Over 34"
  } else if (x > 32) {
    c_yoe = "Over 32"
  } else if (x > 30) {
    c_yoe = "Over 30"
  } else if (x > 28) {
    c_yoe = "Over 28"
  } else if (x > 26) {
    c_yoe = "Over 26"
  } else if (x > 24) {
    c_yoe = "Over 24"
  } else if (x > 22) {
    c_yoe = "Over 22"
  } else if (x > 20) {
    c_yoe = "Over 20"
  } else if (x > 18) {
    c_yoe = "Over 18"
  } else if (x > 16) {
    c_yoe = "Over 16"
  } else if (x > 14) {
    c_yoe = "Over 14"
  } else if (x > 12) {
    c_yoe = "Over 12"
  } else if (x > 10) {
    c_yoe = "Over 10"
  } else if (x > 8) {
    c_yoe = "Over 8"
  } else if (x > 6) {
    c_yoe = "Over 6"
  } else if (x > 4) {
    c_yoe = "Over 4"
  } else if (x > 3) {
    c_yoe = "Over 3"
  } else if (x > 2) {
    c_yoe = "Over 2"
  }
  return(c_yoe)
}


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate pensions:
#   if I retired today
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate current years of service
c_yos <- round(time_length(difftime(today(),pebd),"years"), digits = 2)

#calculate current retirement percentage
c_pension_percentage <- round(c_yos * 0.025, digits = 2)

#calculate average pay for last 36 months
#create a historical 36 month list
historical_36_months <- format(seq(today()-(365 * 3), today() %m-% months(1), by = 'month'), "%Y-%m")

#calculate historical years of service
historical_yos <- round(time_length(difftime(as.Date(ym(historical_36_months)),pebd),"years"),digits = 2)

hist_year <- seq(1:36)
hist_yoe <- seq(1:36)
hist_rank <- seq(1:36)
for (i in 1:36) {
  if (historical_36_months[i] < format(c_dor, "%Y-%m")) {
    hist_rank[i] <- paste0("O",as.numeric(str_sub(c_rank,2,2)) - 1)
  } else {
    hist_rank[i] <- c_rank
  }
  hist_yoe[i] <- calculate_yoe(historical_yos[i])
  hist_year[i] <- str_sub(historical_36_months[i],1,4)
}
