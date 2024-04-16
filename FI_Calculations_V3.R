
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
sp_500

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")
net_worth

#read in bank transactions from empower
#historical transactions
bank_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw.csv")
bank_raw

#new transactions
bank_new <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw_new.csv")
bank_new

#merge historical and new transactions
bank_merged <- rbind(bank_raw,
                     bank_new)

#keep only distinct entries
bank <- distinct(bank_merged)

#export the new historicals
write_csv(bank, "/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw.csv")

#read in transaction categories
cats <- read_xlsx("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transaction_categories.xlsx") %>%
  rename(transaction_type = `Transaction Type`,
         category = Category)

#change all of the transactions types to lower case
cats$transaction_type <- tolower(cats$transaction_type)

#clean up the text to get rid of special characters and make them lower case
#unique(cats$category)
cats$category <- gsub("\\(","",cats$category)
cats$category <- gsub("\\)","",cats$category)
cats$category <- gsub("-","_",cats$category)
cats$category <- gsub("& ","",cats$category)
cats$category <- gsub(" ","_",cats$category)
cats$category <- gsub("/","_",cats$category)
cats$category <- tolower(cats$category)

#get rid of spaces in names
names(bank) <- sub(" ","_",names(bank))

#change all names to lower
names(bank) <- tolower(names(bank))

# do the same to category and account_name
#unique(bank$category)
bank$category <- gsub("& ","",bank$category)
bank$category <- gsub(" ","_",bank$category)
bank$category <- gsub("/","_",bank$category)
bank$category <- tolower(bank$category)

#unique(bank$account)
bank$account <- gsub(" ","_",bank$account)
bank$account <- tolower(bank$account)

#add transaction type category
bank <- bank %>%
  left_join(cats, by = "category") %>%
  select(!tags)

bank

#required minimum distributions
required_minimum_distributions <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/required_minimum_distributions.xlsx")
required_minimum_distributions

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
rolling_12_months

rolling_13_months <- format(seq(lubridate::floor_date(today(), unit = "month") %m-% months(12),
                                lubridate::floor_date(today(), unit = "month"),
                                by = "month"),
                            "%Y-%m")
rolling_13_months

#current age
c_age <- floor(time_length(difftime(today(),as.Date(mdy("12-26-1978"))),"years"))
c_age

#year I turn 60
year_60 <- year(today()) + 60 - c_age

#estimated EOL
est_eol_age <- 90

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$annual_return))/100
avg_annual_returns

#automatic brokerage investments
c_auto_brokerage_investments <- 1000 * 12
c_auto_brokerage_investments

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
c_yos

#calculate current retirement percentage
c_pension_percentage <- round(c_yos * 0.025, digits = 2)
c_pension_percentage

#calculate average pay for last 36 months
#create a historical 36 month list
historical_36_months <- format(seq(today()%m-% months(36), today() %m-% months(1), by = 'month'), "%Y-%m")
historical_36_months

#calculate historical years of service
historical_yos <- round(time_length(difftime(ceiling_date(as.Date(ym(historical_36_months)),'month') - days(1),pebd),"years"),digits = 2)
historical_yos

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

# hist_monthly_pay
# hist_year
# hist_yoe
# hist_rank

hist_avg_monthly <- round(mean(hist_monthly_pay),digits = 2)
scales::dollar(hist_avg_monthly)

#calculate monthly pension based on pension percentage and historical average monthly pay
c_monthly_gross_pension <- round(c_pension_percentage * hist_avg_monthly, digits = 2)
scales::dollar(c_monthly_gross_pension)

#calculate annual pension
c_annual_gross_pension <- c_monthly_gross_pension * 12
scales::dollar(c_annual_gross_pension)

#calculate annual net pension
c_annual_net_pension <- calculate_net_income(c_annual_gross_pension, std_deduction)
scales::dollar(c_annual_net_pension)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate pensions:
#   estimated retirement 
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# create vector of 36 months based on estimated retirement date
final_36_months <- seq(as.Date(est_retire_date)%m-% months(36),as.Date(est_retire_date)%m-% months(1), by = "month")
final_36_months

#calculate years of service based on 36 month vector
est_yos <- round(time_length(difftime(ceiling_date(as.Date(ymd(final_36_months)),'month') - days(1),pebd),"years"), digits = 2)
max(est_yos)

#calculate pension percentage from estimated years of service
est_pension_percentage <- round(max(est_yos) * 0.025, digits = 3)
est_pension_percentage

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

# est_monthly_pay
# est_year
# est_yoe
# est_rank

est_avg_monthly <- round(mean(est_monthly_pay),digits = 2)
scales::dollar(est_avg_monthly)

#calculate monthly pension based on pension percentage and historical average monthly pay
est_monthly_gross_pension <- round(est_pension_percentage * est_avg_monthly, digits = 2)
scales::dollar(est_monthly_gross_pension)

#calculate annual pension
est_annual_gross_pension <- est_monthly_gross_pension * 12
scales::dollar(est_annual_gross_pension)

#calculate estimated annual net pension
est_annual_net_pension <- calculate_net_income(est_annual_gross_pension, std_deduction)
scales::dollar(est_annual_net_pension)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average expenses for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

expenses <- bank %>%
  filter(account %in% c("signature_visa", "usaa_checking","usaa_savings"),
         !transaction_type %in% c("income"),
         !grepl("savings",bank$category)) %>%
  mutate(amount = amount * -1)

expenses

#change the date to month format
expenses$date <- format(ymd(expenses$date), "%Y-%m")

#keep only the last 12 months worth of expenses
expenses_last_12 <- expenses %>%
  filter(date %in% rolling_12_months)

expenses_last_12 %>%
  filter(category %in% c("rent"))

#calculate average monthly expenses for last 12 months
expenses_last_12_grouped <- expenses_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#ignore august 2023.  Incomplete data
expenses_last_12_grouped <- expenses_last_12_grouped %>%
  filter(date > "2023-08")

avg_monthly_expenses <- round(mean(expenses_last_12_grouped$monthly_amount), digits = 2)
scales::dollar(avg_monthly_expenses)

#calculate average annual expenses (minus housing)
avg_annual_expenses <- avg_monthly_expenses * 12
scales::dollar(avg_annual_expenses)

#keep only the last 13 months worth of expenses to plot
expenses_last_13 <- expenses %>%
  filter(date %in% rolling_13_months)

#calculate average monthly expenses for last 12 months
expenses_last_13_grouped <- expenses_last_13 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#ignore august 2023.  Incomplete data
expenses_last_13_grouped <- expenses_last_13_grouped %>%
  filter(date > "2023-08")

#plot it
ggplot(data = expenses_last_13_grouped,
       aes(x = date, y = monthly_amount),
       fill = "red") +
  geom_hline(aes(yintercept = (avg_monthly_expenses))) +
  geom_col(fill = "red") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = scales::dollar(monthly_amount)),
            #label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount + 1500),
                label = paste0("Average: \n",scales::dollar((avg_monthly_expenses))))) +
  labs(title = "Expenses by Month",subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average income for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

income <- bank %>%
  filter(account %in% c("signature_visa", "usaa_checking","usaa_savings"),
         transaction_type %in% c("income")) %>%
  mutate(amount = amount)

income

#change the date to month format
income$date <- format(income$date, "%Y-%m")

#keep only the last 12 months worth of income
income_last_12 <- income %>%
  filter(date %in% rolling_12_months)

#calculate average monthly income for last 12 months
income_last_12_grouped <- income_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#ignore august 2023 to say consistent with expenses
income_last_12_grouped <- income_last_12_grouped %>%
  filter(date > "2023-08")

avg_monthly_income <- round(mean(income_last_12_grouped$monthly_amount), digits = 2)
scales::dollar(avg_monthly_income)

#calculate average annual income (does not include BAH)
avg_annual_income <- avg_monthly_income * 12
scales::dollar(avg_annual_income)

#keep only the last 13 months worth of income to plot
income_last_13 <- income %>%
  filter(date %in% rolling_13_months)

#calculate average monthly expenses for last 12 months
income_last_13_grouped <- income_last_13 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#ignore august 2023 to stay consistent
income_last_13_grouped <- income_last_13_grouped %>%
  filter(date > "2023-08")

#plot it
ggplot(data = income_last_13_grouped) +
  geom_hline(aes(yintercept = avg_monthly_income)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "green") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount + 1500),
                label = paste0("Average: \n",scales::dollar(avg_monthly_income)))) +
  labs(title = "Income by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# plot income and expenses combined
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
merged_last_13_grouped <- rbind(expenses_last_13_grouped %>%
                                  mutate(transaction_type = "expense"),
                                
                                income_last_13_grouped %>%
                                  mutate(transaction_type = "income"))

#plot it
ggplot(data = merged_last_13_grouped) +
  geom_col(aes(x = date,
               y = monthly_amount,
               group = transaction_type,
               fill = transaction_type),
           position = "dodge") +
  scale_fill_manual(values = c("expense" = "red","income" = "green")) +
  scale_y_continuous(seq(0,13000,500), labels = dollar)+
  theme_minimal() +
  labs(title = "Income and Expenses by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"),
        legend.position = "bottom")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average investments and savings and loan repayment for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
investments <- bank %>%
  filter(account %in% c("elaina_roth_ira","jeff_and_elaina_brokerage","jeff_roth_ira","thrift_savings_plan"),
         !category %in% c("securities_trades"),
         !transaction_type %in% c("income")) %>%
  mutate(type = if_else(account %in% c("thrift_savings_plan","jeff_ira","elaina_ira"),
                        "retirement",
                        "non-retirement"))

investments %>%
  group_by(account) %>% summarise(total = sum(amount))

#change the date to month format
investments$date <- format(investments$date, "%Y-%m")

# #keep only the last 12 months worth of investments
# investments_last_12 <- investments %>%
#   filter(date %in% rolling_12_months)
# 
# #calculate average monthly investments for last 12 months
# investments_last_12_grouped_type <- investments_last_12 %>%
#   group_by(date, type) %>%
#   summarise(monthly_amount = sum(amount))
# 
# investments_last_12_grouped <- investments_last_12 %>%
#   group_by(date) %>%
#   summarise(monthly_amount = sum(amount))
# 
# avg_monthly_investments <- round(mean(investments_last_12_grouped$monthly_amount), digits = 2)
# scales::dollar(avg_monthly_investments)
# 
# #keep only the last 13 months worth of investments to plot
# investments_last_13 <- investments %>%
#   filter(date %in% rolling_13_months)
# 
# #calculate average monthly investments for last 12 months
# investments_last_13_grouped_type <- investments_last_13 %>%
#   group_by(date, type) %>%
#   summarise(monthly_amount = sum(amount))
# 
# investments_last_13_grouped <- investments_last_13 %>%
#   group_by(date) %>%
#   summarise(monthly_amount = sum(amount))
# 
# #plot it
# ggplot(data = investments_last_13_grouped_type,
#        aes(x = date, y = monthly_amount, fill = type)) +
#   geom_hline(aes(yintercept = avg_monthly_investments)) +
#   geom_col() +
#   theme_minimal() +
#   geom_text(aes(label = paste0("$", monthly_amount)),
#             position = position_stack()) +
#   geom_text(aes(x = max(date),
#                 y = max(monthly_amount),
#                 label = paste0("Average: \n",scales::dollar(avg_monthly_investments)))) +
#   labs(title = "Investments by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_line(colour = "grey",
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.text.x = element_text(angle = -90, vjust = 0),
#         legend.position = "bottom")

#keep only the categories that are savings
savings <- bank %>%
  filter(grepl("savings",bank$category),
         account %in% c("usaa_checking")) %>%
  mutate(amount = amount * -1)
  
#change the date to month format
savings$date <- format(savings$date, "%Y-%m")

# #keep only the last 12 months worth of savings
# savings_last_12 <- savings %>%
#   filter(date %in% rolling_12_months)
# 
# #calculate average monthly savings for last 12 months
# savings_last_12_grouped <- savings_last_12 %>%
#   group_by(date) %>%
#   summarise(monthly_amount = sum(amount))
# 
# avg_monthly_savings <- round(mean(savings_last_12_grouped$monthly_amount), digits = 2)
# scales::dollar(avg_monthly_savings)
# 
# #keep only the last 13 months worth of savings to plot
# savings_last_13 <- savings %>%
#   filter(date %in% rolling_13_months)
# 
# #calculate average monthly expenses for last 13 months
# savings_last_13_grouped <- savings_last_13 %>%
#   group_by(date) %>%
#   summarise(monthly_amount = sum(amount))
# 
# #plot it
# ggplot(data = savings_last_13_grouped) +
#   geom_hline(aes(yintercept = avg_monthly_savings)) +
#   geom_col(aes(x = date, y = monthly_amount),
#            fill = "blue") +
#   theme_minimal() +
#   geom_text(aes(x = date,
#                 y = monthly_amount,
#                 label = paste0("$", monthly_amount)),
#             vjust = -0.5) +
#   geom_text(aes(x = max(date),
#                 y = max(monthly_amount+ 1500),
#                 label = paste0("Average: \n",scales::dollar(avg_monthly_savings)))) +
#   labs(title = "Savings by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_text(angle = -90, vjust = 0),
#         axis.line = element_line(colour = "grey",
#                                  linewidth = 1,
#                                  linetype = "solid"))


#keep only the categories that are loan repayment
loan <- bank %>% 
  filter(grepl("loan",bank$category),
         !grepl("nissan",bank$account)) %>%
  mutate(amount = amount * -1)

#change the date to month format
loan$date <- format(loan$date, "%Y-%m")

# #keep only the last 12 months worth of loan repayment
# loan_last_12 <- loan %>%
#   filter(date %in% rolling_12_months)
# 
# #calculate average monthly loan repayment for last 12 months
# loan_last_12_grouped <- loan_last_12 %>%
#   group_by(date,category) %>%
#   summarise(monthly_amount = sum(amount))
# 
# avg_monthly_loan_repayment <- round(mean(loan_last_12_grouped$monthly_amount), digits = 2)
# scales::dollar(avg_monthly_loan_repayment)
# 
# #keep only the last 13 months worth of loan repayment
# loan_last_13 <- loan %>%
#   filter(date %in% rolling_13_months)
# 
# #calculate average monthly loan repayment for last 13 months
# loan_last_13_grouped <- loan_last_13 %>%
#   group_by(date,category) %>%
#   summarise(monthly_amount = sum(amount))
# 
# #plot it
# ggplot(data = loan_last_13_grouped) +
#   geom_hline(aes(yintercept = avg_monthly_loan_repayment)) +
#   geom_col(aes(x = date, y = monthly_amount,fill = category)) +
#   theme_minimal() +
#   geom_text(aes(x = date,
#                 y = monthly_amount,
#                 label = paste0("$", monthly_amount)),
#             vjust = -0.5) +
#   geom_text(aes(x = min(date),
#                 y = max(monthly_amount),
#                 label = paste0("Average: \n",scales::dollar(avg_monthly_loan_repayment)))) +
#   labs(title = "Loan Repayment by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_text(angle = -90, vjust = 0),
#         axis.line = element_line(colour = "grey",
#                                  linewidth = 1,
#                                  linetype = "solid"))

#calculate total paid back on school loan
school_loans <- loan %>%
  filter(category %in% c("school_loan","loans"))

total_paid <- sum(school_loans$amount)
scales::dollar(total_paid)

#calculate amount remaining
initial_value <- max(net_worth$School_Loan_Elaina *-1)

remaining_school_loan <- initial_value - total_paid
scales::dollar(remaining_school_loan)

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
scales::dollar(c_retirement_total)

c_tsp_amount <- retirement_funds[retirement_funds$date == max(retirement_funds$date),]$tsp

non_retirement_funds <- non_retirement_funds %>%
  mutate(non_retirement_total = vanguard_brokerage + usaa_savings + usaa_checking)

c_non_retirement_total <- non_retirement_funds[non_retirement_funds$date == max(non_retirement_funds$date),]$non_retirement_total
scales::dollar(c_non_retirement_total)

liabilities <- liabilities %>%
  mutate(liabilities_total = usaa_visa + chase_cc + school_loan_elaina + fixed_rate_loan_nissan)

liabilities %>% filter(date == max(date)) %>% select(liabilities_total)

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

scales::dollar(current_net_worth)

#plot the net worth
ggplot(data = net_worth_merged,
       mapping= aes(x=date,
                    y=net_worth_total)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  geom_text(aes(x = min(date),
                y = first_net_worth,
                label = paste0("$", round(first_net_worth/1000),"K")),
            vjust = -1) +
  geom_text(aes(x = max(date),
                y = current_net_worth,
                label = paste0("$", round(current_net_worth/1000),"K")),
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
#   Naive
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate naive FI number based on current average monthly expenses + housing buffer and 
#assumed safe withdrawal rate

#add housing buffer to months before Feb 2024
expenses_last_12_grouped_housing <- expenses_last_12_grouped %>%
  mutate(monthly_amount = if_else(date < "2024-02",
                                  monthly_amount + housing_buffer,
                                  monthly_amount))

avg_monthly_expenses_housing <- mean(expenses_last_12_grouped_housing$monthly_amount)

naive_fi <- (avg_monthly_expenses_housing)*12 * (1 / annual_safe_withdrawal)
scales::dollar(naive_fi)

#calculate years to naive FI based on current net_worth assuming no additional investments
naive_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth

while (sim_invest_value < naive_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) 
  naive_fi_years_no_invest <- naive_fi_years_no_invest + 1
}

naive_fi_years_no_invest

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate FU numbers:
#   With retire today pension
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate naive FI number based on current average monthly expenses + housing buffer, 
#assumed safe withdrawal rate, and pension if I retired today

today_pension_fi <- ((avg_monthly_expenses_housing)*12 - c_annual_net_pension) * (1 / annual_safe_withdrawal)
scales::dollar(today_pension_fi)

#todays pension is worth
scales::dollar(naive_fi - today_pension_fi)

#calculate years to naive FI based on current net_worth assuming no additional investments
today_pension_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth

while (sim_invest_value < today_pension_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) 
  today_pension_fi_years_no_invest <- today_pension_fi_years_no_invest + 1
}

today_pension_fi_years_no_invest

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate FU numbers:
#   With estimated retirement pension
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate naive FI number based on current average monthly expenses + housing buffer, 
#assumed safe withdrawal rate, and estimated pension if I retired at O5

est_pension_fi <- ((avg_monthly_expenses_housing)*12 - est_annual_net_pension) * (1 / annual_safe_withdrawal)
scales::dollar(est_pension_fi)

#what is my est pension worth
scales::dollar(naive_fi - est_pension_fi)

#calculate years to naive FI based on current net_worth assuming no additional investments
est_pension_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth

while (sim_invest_value < est_pension_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) 
  est_pension_fi_years_no_invest <- est_pension_fi_years_no_invest + 1
}

est_pension_fi_years_no_invest

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Build FI Simulation
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# 
# #begin building simulation
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - NO 
# #add additional investments - NO
# #add pension - NO 
# #add additional income - NO
# #add social security - NO
# #subtract expenses - NO 
# #subtract RMD - NO
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     sim_net_worth_value[i] = current_net_worth
#   } else {
#     sim_net_worth_value[i] = sim_net_worth_value[i -1] * (1 + avg_annual_returns)
#   }
# }
# 
# sim1_out <- as_tibble(cbind(run_num,
#                            sim_year,
#                            sim_age,
#                            sim_net_worth_value))
# 
# ggplot(data = sim1_out,
#        aes(x = sim_year,
#            y = sim_net_worth_value)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area() +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,10000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "none") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth")
# 
# #naive simulation just from c_age to est_eol_age beginning separating out retirement and non-retirement accounts
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - NO
# #add additional investments - NO
# #add pension - NO
# #add additional income - NO
# #add social security - NO
# #subtract expenses - NO
# #subtract RMD - NO
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     sim_net_worth_value_retire[i] = c_retirement_total
#     sim_net_worth_value_non_retire[i] = c_non_retirement_total
#     sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#   } else {
#     sim_net_worth_value_retire[i] = sim_net_worth_value_retire[i -1] * (1 + avg_annual_returns)
#     sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#     sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#   }
# }
# 
# sim2_out <- as_tibble(cbind(run_num,
#                            sim_year,
#                            sim_age,
#                            sim_net_worth_value_retire,
#                            sim_net_worth_value_non_retire,
#                            sim_net_worth_value_total))
# 
# tail(sim2_out)
# 
# sim2_out_long <- sim2_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount")
# 
# ggplot(data = sim2_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))
# 
# #add in TSP contributions until Navy retirement
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - YES "tsp"
# #add additional investments - NO
# #add pension - NO
# #add additional income - NO
# #add social security - NO
# #subtract expenses - NO
# #subtract RMD - NO
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# 
# for (i in run_num) {
#   if (i == 1) {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = c_retirement_total
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year <= retire year
# 
#   } else {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = sim_net_worth_value_retire[i -1] * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year >= retire year
#   } #end if i == 1
# } #end for loop
# 
# sim3_out <- as_tibble(cbind(run_num,
#                            sim_year,
#                            sim_age,
#                            sim_net_worth_value_retire,
#                            sim_net_worth_value_non_retire,
#                            sim_net_worth_value_total))
# 
# tail(sim3_out)
# 
# sim3_out_long <- sim3_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount")
# 
# ggplot(data = sim3_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth \nAdd TSP",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))
# 
# 
# #start subtracting estimated annual expenses from non_retirement funds.  When do we run out?
# #assume no post navy employment and no additional investments
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - YES "tsp"
# #add additional investments in Navy - NO
# #add pension - NO
# #add additional income - NO
# #add social security - NO
# #subtract expenses - YES "est_annual_expenses"
# #subtract RMD - NO
# 
 est_annual_expenses <- (avg_monthly_expenses_housing) * 12
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = c_retirement_total
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year <= retire year
#     
#   } else {
#     if (sim_year[i] <= year(est_retire_date)) { #for years that I am still in the Navy
#       sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else { 
#       if (sim_age[i] < 60) { # for years after the navy but before 59.5 years old
#         sim_net_worth_value_retire[i] =  sim_net_worth_value_retire[i -1]  * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] - est_annual_expenses) * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       } else { #for years after the navy and over 60
#         sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] - est_annual_expenses) * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       }
# 
#     } #end if sim_year >= retire year
#   } #end if i == 1
# } #end for loop
# 
# sim4_out <- as_tibble(cbind(run_num,
#                             sim_year,
#                             sim_age,
#                             sim_net_worth_value_retire,
#                             sim_net_worth_value_non_retire,
#                             sim_net_worth_value_total))
# 
# 
# sim4_out_long <- sim4_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount")
# 
# ggplot(data = sim4_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth \nAdd TSP, \nSubtract est expenses",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))
# 
# #assume no post navy employment but now include continued investments until navy retirement
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - YES "tsp"
# #add additional investments in Navy- YES "c_auto_brokerage_investments"
# #add pension - NO
# #add additional income - NO
# #add social security - NO
# #subtract expenses - YES "est_annual_expenses"
# #subtract RMD - NO
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total + (c_auto_brokerage_investments * (1-month(today())/12)) 
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = c_retirement_total
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year <= retire year
#     
#   } else {
#     if (sim_year[i] <= year(est_retire_date)) { #for years that I am still in the Navy
#       sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + c_auto_brokerage_investments) * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else { 
#       if (sim_age[i] < 60) { # for years after the navy but before 59.5 years old
#         sim_net_worth_value_retire[i] =  sim_net_worth_value_retire[i -1]  * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] - est_annual_expenses) * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       } else { #for years after the navy and over 60
#         sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] - est_annual_expenses) * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       }
#       
#     } #end if sim_year >= retire year
#   } #end if i == 1
# } #end for loop
# 
# sim5_out <- as_tibble(cbind(run_num,
#                             sim_year,
#                             sim_age,
#                             sim_net_worth_value_retire,
#                             sim_net_worth_value_non_retire,
#                             sim_net_worth_value_total))
# 
# 
# sim5_out_long <- sim5_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount")
# 
# ggplot(data = sim5_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth \nAdd TSP, additional investments in Navy \nSubtract est expenses",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))
# 
# 
# # add in pension at estimated retirement date
# 
# #start - current age. "c_age"
# #stop - 90  "est_eol_age"
# #add TSP - YES "tsp"
# #add additional investments in Navy- YES "c_auto_brokerage_investments"
# #add pension - YES "est_net_pension"
# #add additional income - NO
# #add social security - NO
# #subtract expenses - YES "est_annual_expenses
# #subtract RMD - NO
# 
 est_net_pension <- est_annual_net_pension
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total + (c_auto_brokerage_investments * (1-month(today())/12)) 
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = c_retirement_total
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year <= retire year
#     
#   } else {
#     if (sim_year[i] <= year(est_retire_date)) { #for years that I am still in the Navy
#       sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + c_auto_brokerage_investments) * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else { 
#       if (sim_age[i] < 60) { # for years after the navy but before 59.5 years old
#         sim_net_worth_value_retire[i] =  sim_net_worth_value_retire[i -1]  * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       } else { #for years after the navy and over 60
#         sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       }
#       
#     } #end if sim_year >= retire year
#   } #end if i == 1
# } #end for loop
# 
# sim6_out <- as_tibble(cbind(run_num,
#                             sim_year,
#                             sim_age,
#                             sim_net_worth_value_retire,
#                             sim_net_worth_value_non_retire,
#                             sim_net_worth_value_total))
# 
# 
# sim6_out_long <- sim6_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount")
# 
# ggplot(data = sim6_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth \nAdd TSP, additional investments in Navy, estimated pension \nSubtract est expenses",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))

#calculate estimated minimum income to make retirement funds last until 60 years of sim_age

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy- YES "c_auto_brokerage_investments"
#add pension - YES "est_net_pension"
#add additional income - YES "minimum_income"
#add social security - NO
#subtract expenses - YES "est_annual_expenses"
#subtract RMD - NO

ending_age <- c_age
minimum_income <- 0

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
      sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses + est_net_pension + minimum_income) * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
  }
  ending_age <- sim_age
  print(ending_age)
  print(minimum_income)
  print(sim_net_worth_value_non_retire)
}

# if (ending_age == 60 & sim_net_worth_value_non_retire < 0) {
#   minimum_income = minimum_income + 5000
# }

ending_age
scales::dollar(minimum_income)

#now lets see how long retirement funds will last using the minimum income from the last sim

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy- YES "c_auto_brokerage_investments"
#add pension - YES "est_net_pension"
#add additional income - YES "minimum_income"
#add social security - NO
#subtract expenses - YES "est_annual_expenses"
#subtract RMD - NO
# 
# run_num <- seq(c_age:est_eol_age)
# sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
# sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
# sim_net_worth_value_retire <- run_num
# sim_net_worth_value_non_retire <- run_num
# sim_net_worth_value_total <- run_num
# 
# for (i in run_num) {
#   if (i == 1) {
#     if (sim_year[i] <= year(est_retire_date)) {
#       sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total + (c_auto_brokerage_investments * (1-month(today())/12)) 
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else {
#       sim_net_worth_value_retire[i] = c_retirement_total
#       sim_net_worth_value_non_retire[i] = c_non_retirement_total
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } #end if sim_year <= retire year
#     
#   } else {
#     if (sim_year[i] <= year(est_retire_date)) { #for years that I am still in the Navy
#       sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
#       sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] + c_auto_brokerage_investments) * (1 + avg_annual_returns)
#       sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#     } else { 
#       if (sim_age[i] < 60) { # for years after the navy but before 59.5 years old
#         sim_net_worth_value_retire[i] =  sim_net_worth_value_retire[i -1]  * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = (sim_net_worth_value_non_retire[i -1] - est_annual_expenses + est_net_pension + minimum_income) * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       } else { #for years after the navy and over 60
#         sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns) 
#         sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
#         sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
#       }
#       
#     } #end if sim_year >= retire year
#   } #end if i == 1
# } #end for loop
# 
# sim7_out <- as_tibble(cbind(run_num,
#                             sim_year,
#                             sim_age,
#                             sim_net_worth_value_retire,
#                             sim_net_worth_value_non_retire,
#                             sim_net_worth_value_total))
# 
# 
# sim7_out_long <- sim7_out %>%
#   select(!sim_net_worth_value_total) %>%
#   pivot_longer(cols = c("sim_net_worth_value_retire","sim_net_worth_value_non_retire"),
#                names_to = "fund_type",
#                values_to = "amount") %>%
#   mutate(fund_type = factor(fund_type, levels = c("sim_net_worth_value_non_retire", "sim_net_worth_value_retire")))
# 
# ggplot(data = sim7_out_long,
#        aes(x = sim_year,
#            y = amount,
#            group = fund_type,
#            fill = fund_type)) +
#   geom_vline(xintercept = year(est_retire_date),
#              color = "blue") +
#   geom_vline(xintercept = year_60,
#              color = "forestgreen") +
#   geom_area(position = position_stack(reverse = T)) +
#   theme_minimal() +
#   scale_y_continuous(seq(-100000,100000000,50000), labels = dollar) +
#   theme(axis.line = element_line(colour = "grey", 
#                                  linewidth = 1,
#                                  linetype = "solid"),
#         axis.title = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Simulated Net Worth",
#        subtitle = "Naive based only on current net worth \nAdd TSP, additional investments in Navy, estimated pension, \nminimum income \nSubtract est expenses",
#        fill = "Fund Type") +
#   scale_fill_discrete(name = "Fund Type", labels = c("Non-Retirement", "Retirement"))


#begin calculating RMDs. Begins year you turn 72.

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy- YES "c_auto_brokerage_investments"
#add pension - YES "est_net_pension"
#add additional income - YES "minimum_income"
#add social security - NO
#subtract expenses - YES "est_annual_expenses"
#subtract RMD - YES "rmd"

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
        sim_pension_minus_exp[i] =  est_net_pension - est_annual_expenses
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
          sim_pension_minus_exp[i] = est_net_pension - est_annual_expenses
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
          sim_pension_minus_exp[i] = est_net_pension - est_annual_expenses
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

sim8_out

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