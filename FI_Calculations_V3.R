
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

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

#read in mint transactions
# bank <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_transactions.csv") %>%
#   select(!c(Labels,Notes))
bank_raw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw.csv")

bank_new <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw_new.csv")

bank_merged <- rbind(bank_raw,
                     bank_new)

bank <- distinct(bank_merged)

write_csv(bank, "/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw.csv")

#read in mint categories
# cats <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_categories.xlsx",
#                    sheet = "reference")
cats <- read_xlsx("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transaction_categories.xlsx") %>%
  rename(transaction_type = `Transaction Type`,
         category = Category)
cats$transaction_type <- tolower(cats$transaction_type)

unique(cats$category)
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
#names(bank_raw) <- tolower(names(bank_raw))

# do the same to category and account_name
unique(bank$category)
bank$category <- gsub("& ","",bank$category)
bank$category <- gsub(" ","_",bank$category)
bank$category <- gsub("/","_",bank$category)
bank$category <- tolower(bank$category)

# unique(bank_raw$category)
# bank_raw$category <- gsub("& ","",bank_raw$category)
# bank_raw$category <- gsub(" ","_",bank_raw$category)
# bank_raw$category <- gsub("/","_",bank_raw$category)
# bank_raw$category <- tolower(bank_raw$category)

unique(bank$account)
bank$account <- gsub(" ","_",bank$account)
bank$account <- tolower(bank$account)

# unique(bank_raw$account)
# bank_raw$account <- gsub(" ","_",bank_raw$account)
# bank_raw$account <- tolower(bank_raw$account)

#add transaction type category

bank <- bank %>%
  left_join(cats, by = "category") %>%
  select(!tags)
# 
# bank_raw <- bank_raw %>%
#   left_join(cats, by = "category") %>%
#   select(!tags)
# 
# bank <- bank_raw

#remove duplicates
bank <- distinct(bank)

#write out the new data set
# write_xlsx(bank,"/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_all.xlsx")

#required minimum distributions
required_minimum_distributions <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/required_minimum_distributions.xlsx")

#load military pay data
pChart2024 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2024_Officer_Pay.csv")
pChart2023 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv")
pChart2022 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2022_Officer_Pay.csv")
pChart2021 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2021_Officer_Pay.csv")
pChart2020 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2020_Officer_Pay.csv")

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

rolling_13_months <- format(seq(lubridate::floor_date(today(), unit = "month") %m-% months(12),
                                lubridate::floor_date(today(), unit = "month"),
                                by = "month"),
                            "%Y-%m")


#current age
c_age <- floor(time_length(difftime(today(),as.Date(mdy("12-26-1978"))),"years"))

#estimated EOL
est_eol_age <- 90

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$annual_return))/100

#automatic brokerage investments
c_auto_brokerage_investments <- 2000 * 12

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
historical_36_months <- format(seq(today()-(365 * 3), today() %m-% months(1), by = 'month'), "%Y-%m")

#calculate historical years of service
historical_yos <- round(time_length(difftime(as.Date(ym(historical_36_months)),pebd),"years"),digits = 2)

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

#calculate monthly pension based on pension percentage and historical average monthly pay
c_monthly_gross_pension <- round(c_pension_percentage * hist_avg_monthly, digits = 2)

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
final_36_months <- seq(as.Date(est_retire_date)%m-% months(36),as.Date(est_retire_date), by = "month")

#calculate years of service based on 36 month vector
est_yos <- round(time_length(difftime(final_36_months,pebd),"years"), digits = 2)
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

est_avg_monthly <- round(mean(est_monthly_pay),digits = 2)

#calculate monthly pension based on pension percentage and historical average monthly pay
est_monthly_gross_pension <- round(est_pension_percentage * est_avg_monthly, digits = 2)

#calculate annual pension
est_annual_gross_pension <- est_monthly_gross_pension * 12
scales::dollar(est_annual_gross_pension)

#calculate estimated annual net pension
est_annual_net_pension <- calculate_net_income(est_annual_gross_pension, std_deduction)
scales::dollar(est_annual_net_pension)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average expenses for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# expenses <- bank %>% filter(transaction_type == "debit",
#                             !hof_category %in% c("transfer","investments"),
#                             !(category %in% c("auto_payment") & description %in% c("USAA LOAN PAYMENT")),
#                             !account_name %in% c("eleanor_savings"))

expenses <- bank %>%
  filter(account %in% c("signature_visa", "usaa_checking","usaa_savings"),
         !transaction_type %in% c("income"),
         !grepl("savings",bank$category)) %>%
  mutate(amount = amount * -1)

#change the date to month format
expenses$date <- format(ymd(expenses$date), "%Y-%m")

#keep only the last 12 months worth of expenses
expenses_last_12 <- expenses %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
expenses_last_12_grouped <- expenses_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

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
# income <- bank %>%
#   filter(transaction_type == "credit",
#          hof_category %in% c("income"))

income <- bank %>%
  filter(account %in% c("signature_visa", "usaa_checking","usaa_savings"),
         transaction_type %in% c("income")) %>%
  mutate(amount = amount)

#change the date to month format
income$date <- format(income$date, "%Y-%m")

#keep only the last 12 months worth of income
income_last_12 <- income %>%
  filter(date %in% rolling_12_months)

#calculate average monthly income for last 12 months
income_last_12_grouped <- income_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

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
# Calculate average investments and savings and loan repayment for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
investments <- bank %>%
  filter(account %in% c("elaina_roth_ira","jeff_and_elaina_brokerage","jeff_roth_ira","thrift_savings_plan"),
         !category %in% c("securities_trades"),
         !transaction_type %in% c("income")) %>%
  mutate(type = if_else(account %in% c("thrift_savings_plan","jeff_ira","elaina_ira"),
                        "retirement",
                        "non-retirement"))

# investments <- bank %>% 
#   filter(hof_category == "investments",
#          #account_name %in% c("jeff_and_elaina_brokerage","thrift_savings_plan"),
#          !category %in% c("dividend_cap_gains"),
#          transaction_type == "credit",
#          !grepl("Settlement",description)) %>%
#   mutate(type = if_else(account_name %in% c("thrift_savings_plan","jeff_ira","elaina_ira"),
#                         "retirement",
#                         "non-retirement"))

investments %>%
  group_by(account) %>% summarise(total = sum(amount))

#change the date to month format
investments$date <- format(investments$date, "%Y-%m")

#keep only the last 12 months worth of investments
investments_last_12 <- investments %>%
  filter(date %in% rolling_12_months)

#calculate average monthly investments for last 12 months
investments_last_12_grouped_type <- investments_last_12 %>%
  group_by(date, type) %>%
  summarise(monthly_amount = sum(amount))

investments_last_12_grouped <- investments_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_investments <- round(mean(investments_last_12_grouped$monthly_amount), digits = 2)
scales::dollar(avg_monthly_investments)

#keep only the last 13 months worth of investments to plot
investments_last_13 <- investments %>%
  filter(date %in% rolling_13_months)

#calculate average monthly investments for last 12 months
investments_last_13_grouped_type <- investments_last_13 %>%
  group_by(date, type) %>%
  summarise(monthly_amount = sum(amount))

investments_last_13_grouped <- investments_last_13 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#plot it
ggplot(data = investments_last_13_grouped_type,
       aes(x = date, y = monthly_amount, fill = type)) +
  geom_hline(aes(yintercept = avg_monthly_investments)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = paste0("$", monthly_amount)),
            position = position_stack()) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_investments)))) +
  labs(title = "Investments by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"),
        axis.text.x = element_text(angle = -90, vjust = 0),
        legend.position = "bottom")

#keep only the categories that are savings
savings <- bank %>%
  filter(grepl("savings",bank$category),
         account %in% c("usaa_checking")) %>%
  mutate(amount = amount * -1)
  
# savings <- bank %>% filter(transaction_type == "credit",
#                            category %in% c("savings")) 

#change the date to month format
savings$date <- format(savings$date, "%Y-%m")

#keep only the last 12 months worth of savings
savings_last_12 <- savings %>%
  filter(date %in% rolling_12_months)

#calculate average monthly savings for last 12 months
savings_last_12_grouped <- savings_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_savings <- round(mean(savings_last_12_grouped$monthly_amount), digits = 2)
scales::dollar(avg_monthly_savings)

#keep only the last 13 months worth of savings to plot
savings_last_13 <- savings %>%
  filter(date %in% rolling_13_months)

#calculate average monthly expenses for last 13 months
savings_last_13_grouped <- savings_last_13 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

#plot it
ggplot(data = savings_last_13_grouped) +
  geom_hline(aes(yintercept = avg_monthly_savings)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "blue") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount+ 1500),
                label = paste0("Average: \n",scales::dollar(avg_monthly_savings)))) +
  labs(title = "Savings by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))


#keep only the categories that are loan repayment
#loan <- bank %>% filter(category %in% c("auto_payment"))
loan <- bank %>% 
  filter(grepl("loan",bank$category),
         !grepl("nissan",bank$account)) %>%
  mutate(amount = amount * -1)

#change the date to month format
loan$date <- format(loan$date, "%Y-%m")

#keep only the last 12 months worth of loan repayment
loan_last_12 <- loan %>%
  filter(date %in% rolling_12_months)

#calculate average monthly loan repayment for last 12 months
loan_last_12_grouped <- loan_last_12 %>%
  group_by(date,category) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_loan_repayment <- round(mean(loan_last_12_grouped$monthly_amount), digits = 2)
scales::dollar(avg_monthly_loan_repayment)

#keep only the last 13 months worth of loan repayment
loan_last_13 <- loan %>%
  filter(date %in% rolling_13_months)

#calculate average monthly loan repayment for last 13 months
loan_last_13_grouped <- loan_last_13 %>%
  group_by(date,category) %>%
  summarise(monthly_amount = sum(amount))

#plot it
ggplot(data = loan_last_13_grouped) +
  geom_hline(aes(yintercept = avg_monthly_loan_repayment)) +
  geom_col(aes(x = date, y = monthly_amount,fill = category)) +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = min(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_loan_repayment)))) +
  labs(title = "Loan Repayment by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

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

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate FU numbers:
#   Naive
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#calculate naive FI number based on current average monthly expenses + housing buffer and 
#assumed safe withdrawal rate

naive_fi <- (avg_monthly_expenses + housing_buffer)*12 * (1 / annual_safe_withdrawal)

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

today_pension_fi <- ((avg_monthly_expenses + housing_buffer)*12 - c_annual_net_pension) * (1 / annual_safe_withdrawal)

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

est_pension_fi <- ((avg_monthly_expenses + housing_buffer)*12 - est_annual_net_pension) * (1 / annual_safe_withdrawal)
scales::dollar(est_pension_fi)

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

#things to note:
# current investment percent until Navy retirement
# burn down retirement account first
# required minimum distributions
# toggle inputs for post Navy pay
# toggle inputs for post Navy investments

#calculate effective tax rate based on pay scale
#calculate current years of service and years of experience
c_yos <- round(time_length(difftime(today(),pebd),"years"),digits = 2)
c_yoe <- calculate_yoe(c_yos)

#calculate jeff gross annual pay based on pay charts (ignore bah because we live in housing)
jeff_monthly_pay_gross <- pChart2023[pChart2023$YOE == c_yoe,c_rank]
jeff_annual_pay_gross = jeff_monthly_pay_gross * 12

#calculate elaina annual pay based on historicals (1099 employee)
elaina_annual_pay_gross <- bank %>%
  filter(category == "elaina_pay" & date >= (today()-365)) %>%
  summarise(elaina_annual_pay = sum(amount))

#total annual gross
total_annual_pay_gross <- jeff_annual_pay_gross + elaina_annual_pay_gross[[1]]

#estimate effective tax rate based on calculated gross and average monthly income
est_effective_tax_rate <- 1 - (avg_monthly_income *12) / total_annual_pay_gross

#calculate annual investment percentage
total_invest_last_12 <- sum(investments_last_12_grouped$monthly_amount) + tsp

est_percent_invested <- round(total_invest_last_12/total_annual_pay_gross, digits = 2)


#begin building simulation

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - NO 
#add additional investments - NO
#add pension - NO 
#add additional income - NO
#add social security - NO
#subtract expenses - NO 
#subtract RMD - NO

run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value <- run_num

for (i in run_num) {
  if (i == 1) {
    sim_net_worth_value[i] = current_net_worth
  } else {
    sim_net_worth_value[i] = sim_net_worth_value[i -1] * (1 + avg_annual_returns)
  }
}

sim_out <- as_tibble(cbind(run_num,
                           sim_year,
                           sim_age,
                           sim_net_worth_value))

tail(sim_out)

#naive simulation just from c_age to est_eol_age beginning separating out retirement and non-retirement accounts

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - NO
#add additional investments - NO
#add pension - NO
#add additional income - NO
#add social security - NO
#subtract expenses - NO
#subtract RMD - NO

run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value_retire <- run_num
sim_net_worth_value_non_retire <- run_num
sim_net_worth_value_total <- run_num

for (i in run_num) {
  if (i == 1) {
    sim_net_worth_value_retire[i] = c_retirement_total
    sim_net_worth_value_non_retire[i] = c_non_retirement_total
    sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
  } else {
    sim_net_worth_value_retire[i] = sim_net_worth_value_retire[i -1] * (1 + avg_annual_returns)
    sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
    sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
  }
}

sim_out <- as_tibble(cbind(run_num,
                           sim_year,
                           sim_age,
                           sim_net_worth_value_retire,
                           sim_net_worth_value_non_retire,
                           sim_net_worth_value_total))

tail(sim_out)

#add in TSP contributions until Navy retirement

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments - NO
#add pension - NO
#add additional income - NO
#add social security - NO
#subtract expenses - NO
#subtract RMD - NO

run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value_retire <- run_num
sim_net_worth_value_non_retire <- run_num
sim_net_worth_value_total <- run_num


for (i in run_num) {
  if (i == 1) {
    if (sim_year[i] <= year(est_retire_date)) {
      sim_net_worth_value_retire[i] = c_retirement_total + (tsp * (1-month(today())/12))
      sim_net_worth_value_non_retire[i] = c_non_retirement_total
      sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
    } else {
      sim_net_worth_value_retire[i] = c_retirement_total
      sim_net_worth_value_non_retire[i] = c_non_retirement_total
      sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
    } #end if sim_year <= retire year

  } else {
    if (sim_year[i] <= year(est_retire_date)) {
      sim_net_worth_value_retire[i] = (sim_net_worth_value_retire[i -1] + tsp) * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
      sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
    } else {
      sim_net_worth_value_retire[i] = sim_net_worth_value_retire[i -1] * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire[i] = sim_net_worth_value_non_retire[i -1] * (1 + avg_annual_returns)
      sim_net_worth_value_total[i] = sim_net_worth_value_retire[i] + sim_net_worth_value_non_retire[i]
    } #end if sim_year >= retire year
  } #end if i == 1
} #end for loop

sim_out <- as_tibble(cbind(run_num,
                           sim_year,
                           sim_age,
                           sim_net_worth_value_retire,
                           sim_net_worth_value_non_retire,
                           sim_net_worth_value_total)) %>%
  mutate(sim_net_worth_value_retire = scales::dollar(sim_net_worth_value_retire),
         sim_net_worth_value_non_retire = scales::dollar(sim_net_worth_value_non_retire),
         sim_net_worth_value_total = scales::dollar(sim_net_worth_value_total))

tail(sim_out)

#start subtracting estimated annual expenses from non_retirement funds.  When do we run out?
#assume no post navy employment and no additional investments

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy - NO
#add pension - NO
#add additional income - NO
#add social security - NO
#subtract expenses - YES "est_annual_expenses"
#subtract RMD - NO

est_annual_expenses <- (avg_monthly_expenses + housing_buffer) * 12

sim_year <- year(today())
sim_age <- c_age
sim_net_worth_value_retire <- c_retirement_total
sim_net_worth_value_non_retire <- c_non_retirement_total
sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire

while (sim_net_worth_value_non_retire >= 0) {
  if (sim_year == year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = sim_net_worth_value_retire + (tsp * (1-month(today())/12))
    sim_net_worth_value_non_retire = sim_net_worth_value_non_retire
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  } else if (sim_year != year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = (sim_net_worth_value_retire + tsp) * (1 + avg_annual_returns)
    sim_net_worth_value_non_retire = sim_net_worth_value_non_retire * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  } else if (sim_year != year(today()) & sim_year > year(est_retire_date)) {
    sim_net_worth_value_retire = sim_net_worth_value_retire * (1 + avg_annual_returns)
    sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses) * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
}

sim_year
sim_age
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)


#assume no post navy employment but now include continued investments until navy retirement

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy- YES "c_auto_brokerage_investments"
#add pension - NO
#add additional income - NO
#add social security - NO
#subtract expenses - YES "est_annual_expenses"
#subtract RMD - NO

sim_year <- year(today())
sim_age <- c_age
sim_net_worth_value_retire <- c_retirement_total
sim_net_worth_value_non_retire <- c_non_retirement_total
sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire

while (sim_net_worth_value_non_retire >= 0) {
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
    sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses) * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
}

sim_year
sim_age
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)


# add in pension at estimated retirement date

#start - current age. "c_age"
#stop - 90  "est_eol_age"
#add TSP - YES "tsp"
#add additional investments in Navy- YES "c_auto_brokerage_investments"
#add pension - YES "est_net_pension"
#add additional income - NO
#add social security - NO
#subtract expenses - YES "est_annual_expenses
#subtract RMD - NO

est_net_pension <- est_annual_net_pension

sim_year <- year(today())
sim_age <- c_age
sim_net_worth_value_retire <- c_retirement_total
sim_net_worth_value_non_retire <- c_non_retirement_total
sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire

while (sim_net_worth_value_non_retire >= 0) {
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
    sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
}

sim_year
sim_age
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)

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

ending_age
scales::dollar(minimum_income)
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)

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


sim_year <- year(today())
sim_age <- c_age
sim_net_worth_value_retire <- c_retirement_total
sim_net_worth_value_non_retire <- c_non_retirement_total
sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire

while (sim_age <= est_eol_age) {
  #this years
  if (sim_year == year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = sim_net_worth_value_retire + (tsp * (1-month(today())/12)) 
    sim_net_worth_value_non_retire = sim_net_worth_value_non_retire + (c_auto_brokerage_investments * (1-month(today())/12)) 
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
  #navy years not this year
  else if (sim_year != year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = (sim_net_worth_value_retire + tsp) * (1 + avg_annual_returns)
    sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire + c_auto_brokerage_investments) * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
  #post navy
  else if (sim_year != year(today()) & sim_year > year(est_retire_date)) {
    if (sim_age < 60) {
      sim_net_worth_value_retire = sim_net_worth_value_retire * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses + est_net_pension + minimum_income) * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
    else if (sim_age >= 60) {
      sim_net_worth_value_retire = (sim_net_worth_value_retire - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = sim_net_worth_value_non_retire * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
  }
}

sim_year
sim_age
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)

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



sim_year <- year(today())
sim_age <- c_age
sim_net_worth_value_retire <- c_retirement_total
sim_net_worth_value_non_retire <- c_non_retirement_total
sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire

while (sim_age <= est_eol_age) {
  #this years
  if (sim_year == year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = sim_net_worth_value_retire + (tsp * (1-month(today())/12)) 
    sim_net_worth_value_non_retire = sim_net_worth_value_non_retire + (c_auto_brokerage_investments * (1-month(today())/12)) 
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
  #navy years not this year
  else if (sim_year != year(today()) & sim_year <= year(est_retire_date)) {
    sim_net_worth_value_retire = (sim_net_worth_value_retire + tsp) * (1 + avg_annual_returns)
    sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire + c_auto_brokerage_investments) * (1 + avg_annual_returns)
    sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
    sim_year = sim_year + 1
    sim_age = sim_age + 1
  }
  #post navy
  else if (sim_year != year(today()) & sim_year > year(est_retire_date)) {
    if (sim_age < 60) {
      sim_net_worth_value_retire = sim_net_worth_value_retire * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = (sim_net_worth_value_non_retire - est_annual_expenses + est_net_pension + minimum_income) * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
    else if (sim_age >= 60) {
      sim_net_worth_value_retire = (sim_net_worth_value_retire - est_annual_expenses + est_net_pension) * (1 + avg_annual_returns)
      sim_net_worth_value_non_retire = sim_net_worth_value_non_retire * (1 + avg_annual_returns)
      sim_net_worth_value_total <- sim_net_worth_value_non_retire + sim_net_worth_value_retire
      sim_year = sim_year + 1
      sim_age = sim_age + 1
    }
  }
}

sim_year
sim_age
scales::dollar(sim_net_worth_value_retire)
scales::dollar(sim_net_worth_value_non_retire)
scales::dollar(sim_net_worth_value_total)

