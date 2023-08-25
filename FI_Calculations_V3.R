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
# Calculate average investments savings and loan repayment for last 12 months
# Calculate current net worth
# Calculate FU numbers:
#   Naive
#   With retire today pension
#   With estimated retirement pension
# Build FI simulation

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
pChart2020 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2020_Officer_Pay.csv")

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

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$annual_return))/100

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
      (x-22000)*(1-0.12) 
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
  } else {
    hist_monthly_pay[i] <- "ERROR"
  }
}

hist_avg_monthly <- round(mean(hist_monthly_pay),digits = 2)

#calculate monthly pension based on pension percentage and historical average monthly pay
c_monthly_gross_pension <- round(c_pension_percentage * hist_avg_monthly, digits = 2)

#calculate annual pension
c_annual_gross_pension <- c_monthly_gross_pension * 12

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate pensions:
#   estimated retirement 
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# create vector of 36 months based on estimated retirement date
final_36_months <- seq(as.Date(est_retire_date)%m-% months(36),as.Date(est_retire_date), by = "month")

#calculate years of service based on 36 month vector
est_yos <- round(time_length(difftime(final_36_months,pebd),"years"), digits = 2)

#calculate pension percentage from estimated years of service
est_pension_percentage <- round(max(est_yos) * 0.025, digits = 3)

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
  } else if (est_year[i] >= "2023") {
    temp_val <- pChart2023 %>% filter(YOE == est_yoe[i]) %>% select(est_rank[i])
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


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average expenses for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#make all of the column names lower case
names(bank) <- tolower(names(bank))

#make category lower case
bank$category <- tolower(bank$category)

#take a look at the distinct categories
unique(bank$category)

#keep only the categories that are expenses
expense_categories <- c("recreation",
                        "groceries",
                        "insurance",
                        "bank_transaction",
                        "eleanor_school",
                        "pets",
                        "taxes",
                        "health_care",
                        "miscellaneous",
                        "restaurant",
                        "transportation",
                        "professional_expenses",
                        "housing",
                        "personal_contributions_gifts",
                        "home_office_phone_internet",
                        "chase_cc_payment")

expenses <- bank %>% filter(category %in% expense_categories)

#change the date to month format
expenses$date <- format(expenses$date, "%Y-%m")


#keep only the last 12 months worth of expenses
expenses_last_12 <- expenses %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
expenses_last_12_grouped <- expenses_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)*-1)

avg_monthly_expenses <- round(mean(expenses_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = expenses_last_12_grouped %>% mutate(monthly_amount = monthly_amount),
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
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar((avg_monthly_expenses))))) +
  labs(title = "Expenses by Month",subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average income for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#keep only the categories that are expenses
income_categories <- c("jeff_pay",
                       "miscellaneous_income",
                       "elaina_pay")

income <- bank %>% filter(category %in% income_categories)

#change the date to month format
income$date <- format(income$date, "%Y-%m")


#keep only the last 12 months worth of expenses
income_last_12 <- income %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
income_last_12_grouped <- income_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_income <- round(mean(income_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = income_last_12_grouped) +
  geom_hline(aes(yintercept = avg_monthly_income)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "green") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_income)))) +
  labs(title = "Income by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average investments and savings and loan repayment for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#keep only the categories that are investments
investments <- bank %>% filter(category %in% c("investment"))

#change the date to month format
investments$date <- format(investments$date, "%Y-%m")


#keep only the last 12 months worth of expenses
investments_last_12 <- investments %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
investments_last_12_grouped <- investments_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount) * -1)

avg_monthly_investments <- round(mean(investments_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = investments_last_12_grouped) +
  geom_hline(aes(yintercept = avg_monthly_investments)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "purple") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_investments)))) +
  labs(title = "Investments by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#keep only the categories that are savings
savings <- bank %>% filter(source %in% ("USAA_Savings") & category %in% c("savings"))

#change the date to month format
savings$date <- format(savings$date, "%Y-%m")

#keep only the last 12 months worth of expenses
savings_last_12 <- savings %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
savings_last_12_grouped <- savings_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_savings <- round(mean(savings_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = savings_last_12_grouped) +
  geom_hline(aes(yintercept = avg_monthly_savings)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "blue") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_savings)))) +
  labs(title = "Savings by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))


#keep only the categories that are loan repayment
loan <- bank %>% filter(category %in% c("loan_repayment"))

#change the date to month format
loan$date <- format(loan$date, "%Y-%m")

#keep only the last 12 months worth of expenses
loan_last_12 <- loan %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
loan_last_12_grouped <- loan_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount) * -1)

avg_monthly_loan_repayment <- round(mean(loan_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = loan_last_12_grouped) +
  geom_hline(aes(yintercept = avg_monthly_loan_repayment)) +
  geom_col(aes(x = date, y = monthly_amount),
           fill = "orange") +
  theme_minimal() +
  geom_text(aes(x = date,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  geom_text(aes(x = max(date),
                y = max(monthly_amount),
                label = paste0("Average: \n",scales::dollar(avg_monthly_loan_repayment)))) +
  labs(title = "Loan Repayment by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
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

c_retirement_total <- retirement_funds %>% filter(date == max(date)) %>% select(retirement_total)

non_retirement_funds <- non_retirement_funds %>%
  mutate(non_retirement_total = vanguard_brokerage + usaa_savings + usaa_checking)

c_non_retirement_total <- non_retirement_funds %>% filter(date == max(date)) %>% select(non_retirement_total)

liabilities <- liabilities %>%
  mutate(liabilities_total = usaa_visa + chase_cc + school_loan_elaina + fixed_rate_loan_nissan)

#merge the totals together into a single data table
net_worth_merged <- retirement_funds %>% select(date, retirement_total) %>%
  left_join(non_retirement_funds %>% select(date, non_retirement_total), by = "date") %>%
  left_join(liabilities %>% select(date, liabilities_total), by = "date")

#calculate net worth total
net_worth_merged <- net_worth_merged %>%
  mutate(net_worth_total = retirement_total + non_retirement_total + liabilities_total)

#use max net worth to calculate a max value for the y access
y_max <- plyr::round_any(max(net_worth_merged$net_worth_total) + 50000, 50000, f=ceiling)

first_net_worth <- net_worth_merged %>% filter(date == min(date)) %>% select(net_worth_total)
current_net_worth <- net_worth_merged %>% filter(date == max(date)) %>% select(net_worth_total)

#plot the net worth
ggplot(data = net_worth_merged,
       mapping= aes(x=date,
                    y=net_worth_total)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  geom_text(aes(x = min(date),
                y = first_net_worth[[1]],
                label = paste0("$", round(first_net_worth[[1]]/1000),"K")),
            vjust = -1) +
  geom_text(aes(x = max(date),
                y = current_net_worth[[1]],
                label = paste0("$", round(current_net_worth[[1]]/1000),"K")),
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
sim_invest_value <- current_net_worth[[1]]

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

today_pension_fi <- ((avg_monthly_expenses + housing_buffer)*12 - (calculate_net_income(c_annual_gross_pension))) * (1 / annual_safe_withdrawal)

#calculate years to naive FI based on current net_worth assuming no additional investments
today_pension_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth[[1]]

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

est_pension_fi <- ((avg_monthly_expenses + housing_buffer)*12 - (calculate_net_income(est_annual_gross_pension))) * (1 / annual_safe_withdrawal)

#calculate years to naive FI based on current net_worth assuming no additional investments
est_pension_fi_years_no_invest <- 0
sim_invest_value <- current_net_worth[[1]]

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
jeff_monthly_pay_gross <- pChart2023 %>% filter(YOE == c_yoe) %>% select(all_of(c_rank))
jeff_annual_pay_gross = jeff_monthly_pay_gross[[1]] * 12

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
#naive simulation just from c_age to est_eol_age with just the current net_worth growing

run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value <- run_num

for (i in run_num) {
  if (i == 1) {
    sim_net_worth_value[i] = current_net_worth[[1]]
  } else {
    sim_net_worth_value[i] = sim_net_worth_value[i -1] * (1 + avg_annual_returns)
  }
}

sim_out <- as_tibble(cbind(run_num,
                           sim_year,
                           sim_age,
                           sim_net_worth_value))

#naive simulation just from c_age to est_eol_age beginning separating out retirement and non-retirement accounts
run_num <- seq(c_age:est_eol_age)
sim_year <- seq(year(today()),year(today()) + est_eol_age - c_age, by = 1)
sim_age <- seq(from = c_age, to = est_eol_age, by = 1)
sim_net_worth_value_retire <- run_num
sim_net_worth_value_non_retire <- run_num
sim_net_worth_value_total <- run_num

for (i in run_num) {
  if (i == 1) {
    sim_net_worth_value_retire[i] = c_retirement_total[[1]]
    sim_net_worth_value_non_retire[i] = c_non_retirement_total[[1]]
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

sim_out
