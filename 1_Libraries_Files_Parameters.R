#libraries
library(tidyverse)
library(scales)
library(lubridate)
library(readxl)

#load files
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

bank <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/Expenses.xlsx",
                   sheet = "Transactions")

#parameters
current_age <- 44
end_age <- 95
num_years <- end_age - current_age

