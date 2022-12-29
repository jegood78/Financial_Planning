#load libraries
library(tidyverse)
library(scales)
library(data.table)
library(lubridate)

#Load networth.csv file to serve as base for calculations
nw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

#Convert the date field to a date
nw <- mutate(nw, Date = as.Date(Date,format = "%m/%d/%y"))

#make all column names lower case
names(nw) <- tolower(names(nw))

#keep most recent date and only the investment accounts
current_invest <- nw %>%
  filter(date == max(date)) %>%
  select(date,
         tsp,
         vanguard_brokerage,
         vanguard_ira_jeff,
         vanguard_ira_elaina)

##############################################################################
#Calculate returns for individual accounts using networth.csv as base and 
#aggregate the amounts.  Use the actual historical returns timeline and not
#random sampling.  Calculate until age 95.
##############################################################################

#calculate current age
birthyear <- 1978
age <- year(today()) - birthyear

#calculate number of years until age 95
years <- 95 - age

#load historical S&P 500 returns
sp500 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/sp-500-historical-annual-returns.csv")

#build a loop to calculate returns for the total number of years beginning each 
#year of the historical data that allows for the total number of years
start_year <- min(sp500$year)

end_year <- max(sp500$year) - years

yr <- start_year
while (yr <= end_year) {
  for (i in 1:years) {
    print(paste0(yr,"-",i))
  } #end for loop
  yr = yr + 1
} #end while loop

##############################################################################
#Include tsp and automatic monthly contributions while in the navy
##############################################################################

##############################################################################
#Include Required Minimum Distributions (RMDs)
##############################################################################

##############################################################################
#Include post navy annual contributions?
##############################################################################

##############################################################################
#Include annual withdrawals for expenses?
##############################################################################
