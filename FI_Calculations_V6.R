#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load libraries
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(writexl)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load files
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#read in table of historical S&P 500 returns
sp_500 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/sp-500-historical-annual-returns.csv")
#sp_500

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")
#net_worth

#tsp monthly
tsp_pre_032024 <- 2285
tsp_post_032024 <- 1942

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#checking
checking_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_checking_raw.csv")

#keep only posted transactions
checking_raw <- checking_raw %>%
  filter(Status == "Posted")

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

#keep only posted transactions
savings_raw <- savings_raw %>%
  filter(Status == "Posted")

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

#keep only posted transactions
visa_raw <- visa_raw %>%
  filter(Status == "Posted")

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
bank %>% group_by(key) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(count > 1)

#remove duplicates
bank <- distinct(bank)

#check duplicates by 
bank %>% group_by(key) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(count > 1)

#export the new historicals
write_csv(bank, "/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/usaa_historical.csv")

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

#group by month and transaction typ
bank_monthly_raw <- bank %>% group_by(month, transaction_type) %>% summarise(monthly_amount = sum(amount))

#separate by transaction type to process

#income.  Add TSP amount back and create flow category == "in"
income_monthly <- bank_monthly_raw %>%
  filter(transaction_type == "income") %>%
  mutate(flow = "in",
         monthly_amount = if_else(month <= "2024-03",
                                  monthly_amount + tsp_pre_032024,
                                  monthly_amount + tsp_post_032024))

#investments.  make amounts positive, add tsp amounts and create flow category == "out
investment_monthly <- bank_monthly_raw %>%
  filter(transaction_type == "investment") %>%
  mutate(flow = "out",
         monthly_amount = if_else(month <= "2024-03",
                                  -1 * monthly_amount + tsp_pre_032024,
                                  -1 * monthly_amount + tsp_post_032024))

#fixed expenses. make amounts positive and create flow category == "out"
fixed_expenses_monthly <- bank_monthly_raw %>%
  filter(transaction_type == "fixed_expenses") %>%
  mutate(flow = "out",
         monthly_amount = -1 * monthly_amount)

#misc expenses. make amounts positive and create flow category == "out"
misc_expenses_monthly <- bank_monthly_raw %>%
  filter(transaction_type == "misc_expenses") %>%
  mutate(flow = "out",
         monthly_amount = -1 * monthly_amount)

#savings. make amounts positive and create flow category == "out"
savings_monthly <- bank_monthly_raw %>%
  filter(transaction_type == "savings") %>%
  mutate(flow = "out")

#put them back together
bank_monthly <- rbind(income_monthly,
                      investment_monthly,
                      savings_monthly,
                      fixed_expenses_monthly,
                      misc_expenses_monthly)

#required minimum distributions
required_minimum_distributions <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/required_minimum_distributions.xlsx")
#required_minimum_distributions

#load military pay data
pChart2024 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2024_Officer_Pay.csv")
#pChart2024

pChart2023 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv")
#pChart2023

pChart2022 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2022_Officer_Pay.csv")
#pChart2022

pChart2021 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2021_Officer_Pay.csv")
#pChart2021

pChart2020 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2020_Officer_Pay.csv")
#pChart2020

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Set user inputs
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#housing buffer for expenses since we don't currently pay for housing
housing_buffer <- 3000

#bah amount to add back into pay
bah <- 2928

#set annual safe withdrawal rate
annual_safe_withdrawal <- 0.04

#set TSP contributions
tsp <- 23000

#set pay entry base date
pebd <- as.Date(mdy("2-19-1999")) #pay entry based date

#set current rank
c_rank <- "O5" 

#set current date of rank
c_dor <- as.Date(mdy("9-1-2023"))

#estimated promotion date
est_promotion <- as.Date(mdy("9-1-2029")) 

#estimated retirement date
est_retire_date <- as.Date(mdy("09-1-2026"))

#create a 12 month list
rolling_12_months <- format(seq(lubridate::floor_date(today(), unit = "month") %m-% months(12),
                                lubridate::floor_date(today(), unit = "month") %m-% months(1),
                                by = "month"),
                            "%Y-%m")
max(rolling_12_months)

rolling_13_months <- format(seq(lubridate::floor_date(today(), unit = "month") %m-% months(12),
                                lubridate::floor_date(today(), unit = "month"),
                                by = "month"),
                            "%Y-%m")
max(rolling_13_months)

#current age
c_age <- floor(time_length(difftime(today(),as.Date(mdy("12-26-1978"))),"years"))
#c_age

#year I turn 60
year_60 <- year(today()) + 60 - c_age

#estimated EOL
est_eol_age <- 90

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$annual_return))/100
paste0("Average annual returns for S&P 500 = ", scales::percent(avg_annual_returns))

#automatic brokerage investments
c_auto_brokerage_investments <- 1000 * 12
paste0("Auto brokerage investment amount = ", scales::dollar(c_auto_brokerage_investments))

#standard deduction married filing jointly
std_deduction <- 29200

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Define functions
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#function to calculate federal taxes based on 2024 married filing jointly tax brackets
calculate_net_income <- function(gross_income, std_deduction){
  
  x <- gross_income - std_deduction #standard deduction
  
  if (x <= 23200) {
    net_pay <- x*(1-0.10)
  } 
  else if (x > 23200 & x <= 94300) {
    net_pay <- 23200*(1-0.10) + 
      (x-23200)*(1-0.12) 
  } 
  else if (x > 94300 & x <= 201050) {
    net_pay <- 23200*(1-0.10) + 
      (94300-23200)*(1-0.12) + 
      (x-94300)*(1-0.22)
  } 
  else if (x > 201050 & x <= 383900) {
    net_pay <- 23200*(1-0.10) + 
      (94300-23200)*(1-0.12) + 
      (201050-94300)*(1-0.22) + 
      (x-201050)*(1-0.24)
  }
  else if (x > 383900 & x <= 487450) {
    net_pay <- 23200*(1-0.10) + 
      (94300-23200)*(1-0.12) + 
      (201050-94300)*(1-0.22) + 
      (383900-201050)*(1-0.24) +
      (x-383900) * (1-0.32)
  }
  else if (x > 487450 & x <= 731200) {
    net_pay <- 23200*(1-0.10) + 
      (94300-23200)*(1-0.12) + 
      (201050-94300)*(1-0.22) + 
      (383900-201050)*(1-0.24) +
      (487450-383900) * (1-0.32) +
      (x-487450) * (1-0.35)
  }
  else if (x > 731200) {
    net_pay <- 23200*(1-0.10) + 
      (94300-23200)*(1-0.12) + 
      (201050-94300)*(1-0.22) + 
      (383900-201050)*(1-0.24) +
      (487450-383900) * (1-0.32) +
      (731200-487450) * (1-0.35) +
      (x-731200) * (1-0.37)
  }
  
  net_pay_final <- net_pay + std_deduction
  
  return(round(net_pay_final))
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
paste0("Current years of service = ", c_yos)

#calculate current retirement percentage
c_pension_percentage <- round(c_yos * 0.025, digits = 2)
paste0("Estimated pension rate if I retired today = ", scales::percent(c_pension_percentage))

#calculate average pay for last 36 months
#create a historical 36 month list
historical_36_months <- format(seq(today()%m-% months(36), today() %m-% months(1), by = 'month'), "%Y-%m")
#historical_36_months

#calculate historical years of service
historical_yos <- round(time_length(difftime(ceiling_date(as.Date(ym(historical_36_months)),'month') - days(1),pebd),"years"),digits = 2)
#historical_yos

#calculate average monthly pay for historical 36 months
hist_monthly_pay <- seq(1:36)
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
  
  if (hist_year[i] == "2020") {
    temp_val <- pChart2020 %>% filter(YOE == hist_yoe[i]) %>% select(hist_rank[i])
    hist_monthly_pay[i] <- temp_val[[1]]
  } else if (hist_year[i] == "2021") {
    temp_val <- pChart2021 %>% filter(YOE == hist_yoe[i]) %>% select(hist_rank[i])
    hist_monthly_pay[i] <- temp_val[[1]]
  } else if (hist_year[i] == "2022") {
    temp_val <- pChart2022 %>% filter(YOE == hist_yoe[i]) %>% select(hist_rank[i])
    hist_monthly_pay[i] <- temp_val[[1]]
  } else if (hist_year[i] == "2023") {
    temp_val <- pChart2023 %>% filter(YOE == hist_yoe[i]) %>% select(hist_rank[i])
    hist_monthly_pay[i] <- temp_val[[1]]
  } else if (hist_year[i] >= "2024") {
    temp_val <- pChart2024 %>% filter(YOE == hist_yoe[i]) %>% select(hist_rank[i])
    hist_monthly_pay[i] <- temp_val[[1]]
  } else {
    hist_monthly_pay[i] <- "ERROR"
  }
}


hist_avg_monthly <- round(mean(hist_monthly_pay),digits = 2)
paste0("Average monthly base pay for last 36 months = ", scales::dollar(hist_avg_monthly))

#calculate monthly pension based on pension percentage and historical average monthly pay
c_monthly_gross_pension <- round(c_pension_percentage * hist_avg_monthly, digits = 2)
paste0("Estimated gross monthly pension if I retired today = ", scales::dollar(c_monthly_gross_pension))

#calculate annual pension
c_annual_gross_pension <- c_monthly_gross_pension * 12
paste0("Estimated gross annual pension of I retired today = ", scales::dollar(c_annual_gross_pension))

#calculate annual net pension
c_annual_net_pension <- calculate_net_income(c_annual_gross_pension, std_deduction)
paste0("Estimated net annual pension if I retired today = ", scales::dollar(c_annual_net_pension))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate pensions:
#   estimated retirement 
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# create vector of 36 months based on estimated retirement date
final_36_months <- seq(as.Date(est_retire_date)%m-% months(36),as.Date(est_retire_date)%m-% months(1), by = "month")
max(final_36_months)

#calculate years of service based on 36 month vector
est_yos <- round(time_length(difftime(ceiling_date(as.Date(ymd(final_36_months)),'month') - days(1),pebd),"years"), digits = 2)
paste0("Estimated years of service when I retire = ", max(est_yos))

#calculate pension percentage from estimated years of service
est_pension_percentage <- round(max(est_yos) * 0.025, digits = 3)
paste0("Estimated pension rate when I retire = ", scales::percent(est_pension_percentage))

#estimate rank, years of experience, and monthly base pay for the 36 months based on current 
#rank and estimated promotion date
est_monthly_pay <- seq(1:36)
est_year <- seq(1:36)
est_yoe <- seq(1:36)
est_rank <- seq(1:36)
for (i in 1:36) {
  
  if (final_36_months[i] >= est_promotion) {
    est_rank[i] <- paste0("O",as.numeric(str_sub(c_rank,2,2)) + 1)
  } else {
    est_rank[i] <- c_rank
  }
  
  est_yoe[i] <- calculate_yoe(est_yos[i])
  est_year[i] <- str_sub(final_36_months[i],1,4)
  
  if (est_year[i] == "2020") {
    temp_val <- pChart2020 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
    est_monthly_pay[i] <- temp_val[[1]]
  } else if (est_year[i] == "2021") {
    temp_val <- pChart2021 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
    est_monthly_pay[i] <- temp_val[[1]]
  } else if (est_year[i] == "2022") {
    temp_val <- pChart2022 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
    est_monthly_pay[i] <- temp_val[[1]]
  } else if (est_year[i] == "2023") {
    temp_val <- pChart2023 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
    est_monthly_pay[i] <- temp_val[[1]]
  } else if (est_year[i] >= "2024") {
    temp_val <- pChart2024 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
    est_monthly_pay[i] <- temp_val[[1]]
  } else {
    est_monthly_pay[i] <- "ERROR"
  }
}

est_avg_monthly <- round(mean(est_monthly_pay),digits = 2)
paste0("Estimated monthly base pay when I retire = ", scales::dollar(est_avg_monthly))

#calculate monthly pension based on pension percentage and historical average monthly pay
est_monthly_gross_pension <- round(est_pension_percentage * est_avg_monthly, digits = 2)
paste0("Estimated gross monthly pension when I retire = ", scales::dollar(est_monthly_gross_pension))

#calculate annual pension
est_annual_gross_pension <- est_monthly_gross_pension * 12
paste0("Estimated gross annual pension when I retire = ", scales::dollar(est_annual_gross_pension))

#calculate estimated annual net pension
est_annual_net_pension <- calculate_net_income(est_annual_gross_pension, std_deduction)
paste0("Estimated net annual pension when I retire = ", scales::dollar(est_annual_net_pension))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average expenses for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
avg_monthly_expenses_grouped <- bank_monthly %>%
  filter(transaction_type %in% c("fixed_expenses", "misc_expenses")) %>%
  group_by(month) %>%
  summarise(monthly_expenses = sum(monthly_amount)) %>%
  filter(month %in% rolling_12_months) %>%
  summarise(average_monthly_expenses = mean(monthly_expenses))

avg_monthly_expenses <- round(avg_monthly_expenses_grouped[[1]])
paste0("Average monthly expenses (last 12 months) = ", scales::dollar(avg_monthly_expenses))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average income for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
avg_monthly_income_grouped <- bank_monthly %>%
  filter(transaction_type %in% c("income")) %>%
  group_by(month) %>%
  summarise(monthly_income = sum(monthly_amount)) %>%
  filter(month %in% rolling_12_months) %>%
  summarise(average_monthly_income = mean(monthly_income))

avg_monthly_income <- round(avg_monthly_income_grouped[[1]])
paste0("Average monthly income (last 12 months) = ", scales::dollar(avg_monthly_income))
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate Jeff base income from pay charts
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
monthly_base_df <- pChart2024 %>%
  filter(YOE == calculate_yoe(c_yos)) %>%
  select(all_of(c_rank))

monthly_base <- monthly_base_df[[1]]

monthly_net <- round(calculate_net_income(monthly_base * 12, std_deduction) / 12)

monthly_base_income_jeff <- monthly_net + bah
paste0("Average monthly income (Jeff base pay) = ", scales::dollar(monthly_base_income_jeff))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average investments and savings for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# plot income and expenses combined
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#create a visualization
bank_monthly %>%
  filter(transaction_type %in% c("income", "fixed_expenses", "misc_expenses"),
         month %in% rolling_13_months) %>%
  group_by(month, flow) %>%
  summarise(monthly_total = sum(monthly_amount)) %>%
  ggplot(aes(x = month,
           y = monthly_total,
           group = flow,
           fill = flow)) +
  geom_hline(yintercept = monthly_base_income_jeff) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("out" = "red",
                               "in" = "green4")) +
  scale_y_continuous(seq(0,25000,500), labels = dollar)+
  theme_minimal() +
  labs(title = "Income and Expenses by Month",
       subtitle = "Historical 12 Months + Current \nBlack line = Jeff Base Pay",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"),
        legend.position = "bottom")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate current net worth
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#make all the names lower case
names(net_worth) <- tolower(names(net_worth))

#change date format
net_worth$date <- as.Date(mdy(net_worth$date))

#keep only the last year of data
net_worth_redux <- net_worth %>% filter(date >= (today() - 365))

#separate accounts into retirement, non-retirement, liabilities
retirement_funds <- net_worth_redux %>% select(date,
                                               tsp,
                                               vanguard_ira_jeff,
                                               vanguard_ira_elaina)

non_retirement_funds <- net_worth_redux %>% select(date,
                                                   vanguard_brokerage,
                                                   usaa_savings,
                                                   usaa_checking)

liabilities <- net_worth_redux %>% select(date,
                                          usaa_visa,
                                          chase_cc,
                                          school_loan_elaina,
                                          fixed_rate_loan_nissan)

#create a single value per date for each fund type
retirement_funds <- retirement_funds %>%
  mutate(retirement_total = tsp + vanguard_ira_jeff + vanguard_ira_elaina)

c_retirement_total <- retirement_funds[retirement_funds$date == max(retirement_funds$date),]$retirement_total
paste0("Current retirement fund amount = ", scales::dollar(c_retirement_total))

c_tsp_amount <- retirement_funds[retirement_funds$date == max(retirement_funds$date),]$tsp

non_retirement_funds <- non_retirement_funds %>%
  mutate(non_retirement_total = vanguard_brokerage + usaa_savings + usaa_checking)

c_non_retirement_total <- non_retirement_funds[non_retirement_funds$date == max(non_retirement_funds$date),]$non_retirement_total
paste0("Current non-retirement fund amount = ", scales::dollar(c_non_retirement_total))

liabilities <- liabilities %>%
  mutate(liabilities_total = usaa_visa + chase_cc + school_loan_elaina + fixed_rate_loan_nissan)

liability_total <- liabilities %>% filter(date == max(date)) %>% select(liabilities_total)
paste0("Current liability total = ", scales::dollar(liability_total[[1]]))

#merge the totals together into a single data table
net_worth_merged <- retirement_funds %>% select(date, retirement_total) %>%
  left_join(non_retirement_funds %>% select(date, non_retirement_total), by = "date") %>%
  left_join(liabilities %>% select(date, liabilities_total), by = "date")

#calculate net worth total
net_worth_merged <- net_worth_merged %>%
  mutate(net_worth_total = retirement_total + non_retirement_total + liabilities_total) %>%
  filter(date >= lubridate::floor_date(today(), unit = "day") %m-% days(365))

#use max net worth to calculate a max value for the y access
y_max <- plyr::round_any(max(net_worth_merged$net_worth_total) + 50000, 50000, f=ceiling)

first_net_worth <- net_worth_merged[net_worth_merged$date == min(net_worth_merged$date),]$net_worth_total
current_net_worth <- net_worth_merged[net_worth_merged$date == max(net_worth_merged$date),]$net_worth_total

paste0("Current net worth = ", scales::dollar(current_net_worth))

first_date = min(net_worth_merged$date)
last_date = max(net_worth_merged$date)
#plot the net worth
ggplot(data = net_worth_merged,
       mapping= aes(x=date,
                    y=net_worth_total)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  annotate(geom = "text",
           x = first_date,
           y = first_net_worth,
           label = paste0("$", round(first_net_worth/1000),"K"),
           vjust = -1) +
  annotate(geom = "text",
           x = last_date,
           y = current_net_worth,
           label = paste0("$", round(current_net_worth/1000),"K"),
           vjust = -1) +
  theme(axis.line = element_line(colour = "grey", 
                                 linewidth = 1,
                                 linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  expand_limits(y = c(0, y_max)) +
  scale_y_continuous(labels = scales::dollar,breaks = seq(0, y_max, 50000)) +
  labs(title = "Net Worth", subtitle = "Last 12 Months", x = NULL, y = NULL)

#build an area plot
net_worth_merged_long <- net_worth_merged %>%
  select(!net_worth_total) %>%
  pivot_longer(cols = c("retirement_total", "non_retirement_total", "liabilities_total"),
               names_to = "fund_type",
               values_to = "amount")

ggplot(data = net_worth_merged_long,
       aes(x = date,
           y = amount,
           group = fund_type,
           fill = fund_type)) +
  geom_area() +
  theme_minimal() +
  scale_y_continuous(seq(-100000,1000000,50000), labels = dollar) +
  theme(axis.line = element_line(colour = "grey", 
                                 linewidth = 1,
                                 linetype = "solid"),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(title = "Net Worth",
       subtitle = "Last 12 Months",
       fill = "Fund Type") +
  scale_fill_discrete(name = "Fund Type", labels = c("Liabilities", "Non-Retirement", "Retirement"))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate FU numbers:
#   With estimated retirement pension
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate naive FI number based on current average monthly expenses + housing buffer, 
#assumed safe withdrawal rate, and estimated pension if I retired at O5

est_pension_fi <- ((avg_monthly_expenses)*12 - est_annual_net_pension) * (1 / annual_safe_withdrawal)
paste0("Estimated FI number based on calculated pension = ", scales::dollar(est_pension_fi))

#what is my est pension worth
#scales::dollar(naive_fi - est_pension_fi)

#calculate years to naive FI based on current net_worth assuming no additional investments
est_pension_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth

while (sim_invest_value < est_pension_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) 
  est_pension_fi_years_no_invest <- est_pension_fi_years_no_invest + 1
}

paste0(est_pension_fi_years_no_invest, " years to reach FI based on current investments")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Build FI Simulation
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

ending_age <- c_age
minimum_income <- 0
est_annual_expenses <- avg_monthly_expenses * 12

while (ending_age < 60) {
  minimum_income <- minimum_income + 5000
  sim_year <- year(today())
  sim_age <- c_age
  sim_net_worth_value_retire <- c_retirement_total
  sim_net_worth_value_non_retire <- c_non_retirement_total
  sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
  
  while (sim_net_worth_value_non_retire >= 0 & sim_age < 60) {
    if (sim_year == year(today()) & sim_year <= year(est_retire_date)) {
      sim_net_worth_value_retire = sim_net_worth_value_retire + (tsp * (1-month(today())/12))
      sim_net_worth_value_non_retire = sim_net_worth_value_non_retire + (c_auto_brokerage_investments * (1-month(today())/12)) 
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    } else if (sim_year != year(today()) & sim_year <= year(est_retire_date)) {
      sim_net_worth_value_retire = (sim_net_worth_value_retire + tsp) * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire + c_auto_brokerage_investments) * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    } else if (sim_year != year(today()) & sim_year > year(est_retire_date)) {
      sim_net_worth_value_retire = sim_net_worth_value_retire * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses + est_annual_net_pension + minimum_income) * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
  }
  ending_age <- sim_age
  #print(ending_age)
  #print(minimum_income)
  #print(sim_net_worth_value_non_retire)
}

#ending_age
paste0("We must make ", scales::dollar(minimum_income), " per year until I turn 60")

run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value_retire_ira <- run_num
sim_net_worth_tsp <- run_num
sim_req_min_dist <- run_num
sim_pension_minus_exp <- run_num
sim_net_worth_value_non_retire <- run_num
sim_net_worth_value_total <- run_num

# for (i in run_num) {
#   if (sim_age[i] >= 75) {
#     print(sim_age[i])
#     d_period = required_minimum_distributions[required_minimum_distributions$age == sim_age[i],]$distribution_period
#     print(d_period)
#   }
# } 

for (i in run_num) {
  if (i == 1) {
    if (sim_year[i] <= year(est_retire_date)) {
      sim_net_worth_value_retire_ira[i] = c_retirement_total - c_tsp_amount
      sim_req_min_dist[i] = 0
      sim_pension_minus_exp[i] = 0
      sim_net_worth_tsp[i] = c_tsp_amount + (tsp * (1-month(today())/12))
      sim_net_worth_value_non_retire[i] = c_non_retirement_total + (c_auto_brokerage_investments * (1-month(today())/12)) 
      sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
    } else {
      sim_net_worth_value_retire_ira[i] = c_retirement_total - c_tsp_amount
      sim_req_min_dist[i] = 0
      sim_pension_minus_exp[i] = 0
      sim_net_worth_tsp[i] = c_tsp_amount
      sim_net_worth_value_non_retire[i] = c_non_retirement_total
      sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
    } #end if sim_year <= retire year
    
  } else {
    if (sim_year[i] <= year(est_retire_date)) { #for years that I am still in the Navy
      sim_net_worth_value_retire_ira[i] = (sim_net_worth_value_retire_ira[i -1]) * (1 + avg_annual_returns)
      sim_req_min_dist[i] = 0
      sim_pension_minus_exp[i] = 0
      sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1] + tsp) * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + c_auto_brokerage_investments) * (1 + avg_annual_returns)
      sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
    } else { 
      if (sim_age[i] < 60) { # for years after the navy but before 59.5 years old
        sim_pension_minus_exp[i] =  est_annual_net_pension - est_annual_expenses
        sim_req_min_dist[i] = 0
        if (sim_pension_minus_exp[i] < 0) {
          sim_net_worth_value_retire_ira[i] =  sim_net_worth_value_retire_ira[i -1]  * (1 + avg_annual_returns) 
          sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1]) * (1 + avg_annual_returns)
          sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + sim_pension_minus_exp[i] + minimum_income) * (1 + avg_annual_returns)
          sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
        } else {
          sim_net_worth_value_retire_ira[i] =  sim_net_worth_value_retire_ira[i -1]  * (1 + avg_annual_returns) 
          sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1]) * (1 + avg_annual_returns)
          sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + sim_pension_minus_exp[i] + minimum_income) * (1 + avg_annual_returns)
          sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
        }
        
      } else { #for years after the navy and over 60
        if (sim_age[i] < 75) { #for the years before rmd
          sim_req_min_dist[i] = 0
          sim_pension_minus_exp[i] = est_annual_net_pension - est_annual_expenses
          if(sim_pension_minus_exp[i] < 0){
            sim_net_worth_value_retire_ira[i] = (sim_net_worth_value_retire_ira[i -1]) * (1 + avg_annual_returns) 
            sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1] + sim_pension_minus_exp[i]) * (1 + avg_annual_returns)
            sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
            sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
          } else {
            sim_net_worth_value_retire_ira[i] = (sim_net_worth_value_retire_ira[i -1]) * (1 + avg_annual_returns) 
            sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1] ) * (1 + avg_annual_returns)
            sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + sim_pension_minus_exp[i]) * (1 + avg_annual_returns)
            sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
          }
        } else {
          sim_req_min_dist[i] = sim_net_worth_tsp[i-1]/required_minimum_distributions[required_minimum_distributions$age == sim_age[i],]$distribution_period
          sim_pension_minus_exp[i] = est_annual_net_pension - est_annual_expenses
          if(sim_pension_minus_exp[i] < 0){
            sim_net_worth_value_retire_ira[i] = (sim_net_worth_value_retire_ira[i -1]) * (1 + avg_annual_returns) 
            sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1] - sim_req_min_dist[i]+ sim_pension_minus_exp[i]) * (1 + avg_annual_returns)
            sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + sim_req_min_dist[i]+ sim_pension_minus_exp[i]) * (1 + avg_annual_returns)
            sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
          } else {
            sim_net_worth_value_retire_ira[i] = (sim_net_worth_value_retire_ira[i -1]) * (1 + avg_annual_returns) 
            sim_net_worth_tsp[i] = (sim_net_worth_tsp[i-1] - sim_req_min_dist[i] ) * (1 + avg_annual_returns)
            sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + sim_pension_minus_exp[i] + sim_req_min_dist[i]) * (1 + avg_annual_returns)
            sim_net_worth_value_total[i] = sim_net_worth_value_retire_ira[i] + sim_net_worth_tsp[i] + sim_net_worth_value_non_retire[i]
          }
        }
        
      }
      
    } #end if sim_year >= retire year
  } #end if i == 1
} #end for loop

sim8_out <- as_tibble(cbind(run_num,
                            sim_year,
                            sim_age,
                            sim_net_worth_value_retire_ira,
                            sim_net_worth_tsp,
                            sim_req_min_dist,
                            sim_pension_minus_exp,
                            sim_net_worth_value_non_retire,
                            sim_net_worth_value_total))

#sim8_out

sim8_out_long <- sim8_out %>%
  select(!sim_net_worth_value_total) %>%
  pivot_longer(cols = c("sim_net_worth_value_retire_ira","sim_net_worth_tsp","sim_net_worth_value_non_retire"),
               names_to = "fund_type",
               values_to = "amount") %>%
  mutate(fund_type = factor(fund_type, levels = c("sim_net_worth_value_non_retire", "sim_net_worth_value_retire_ira", "sim_net_worth_tsp")))

ggplot(data = sim8_out_long,
       aes(x = sim_year,
           y = amount,
           group = fund_type,
           fill = fund_type)) +
  geom_vline(xintercept = year(est_retire_date),
             color = "blue") +
  geom_vline(xintercept = year_60,
             color = "forestgreen") +
  geom_area(position = position_stack(reverse = T)) +
  theme_minimal() +
  scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
  theme(axis.line = element_line(colour = "grey", 
                                 linewidth = 1,
                                 linetype = "solid"),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(title = "Simulated Net Worth",
       subtitle = "Naive based only on current net worth \nAdd TSP, additional investments in Navy, estimated pension, \nminimum income \nSubtract est expenses, rmd",
       fill = "Fund Type") +
  scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement","IRA", "TSP"))

paste0("Simulated final net worth value = ", scales::dollar(tail(sim8_out$sim_net_worth_value_total,1)))
