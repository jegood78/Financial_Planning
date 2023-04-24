#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#libraries

library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#read in files

#read in table of historical S&P 500 returns
sp_500 <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/sp-500-historical-annual-returns.csv")

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

#read expenses file in 
bank <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/Expenses.xlsx",
                   sheet = "Transactions")

#load military pay data
pChart <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#set user inputs

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
rolling_months <- format(seq(today()-365, today() %m-% months(1), by = 'month'), "%Y-%m")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#functions
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
#calculate base values

###########################################
# parse income data

#change all of the names to lower case
names(bank) <- tolower(names(bank))

#change date from date-time to date
bank$date <- as.Date(bank$date)

#rename category to subcategory and shift to lower case
bank <- bank %>%
  mutate(subcategory = category) %>%
  select(!category)

bank$subcategory <- tolower(bank$subcategory)

#add month column
bank$month <- format(bank$date, "%Y-%m")

#add higher level category
bank %>% distinct(subcategory)
bank <- bank %>%
  mutate(category = ifelse(subcategory %in% c("groceries",
                                              "restaurant"),
                           "food",
                           ifelse(subcategory %in% c("transportation"),
                                  "transportation",
                                  ifelse(subcategory %in% c("home_office_phone_internet",
                                                            "housing"),
                                         "housing",
                                         ifelse(subcategory %in% c("chase_cc_payment",
                                                                   "usaa_cc_payment"),
                                                "credit_cards",
                                                ifelse(subcategory %in% c("jeff_pay",
                                                                          "elaina_pay",
                                                                          "miscellaneous_income"),
                                                       "income", 
                                                       ifelse(subcategory %in% c("investment"),
                                                              "investment",
                                                              if_else(subcategory %in% c("pets"),
                                                                      "pets",
                                                                      if_else(subcategory %in% c("loan_repayment"),
                                                                              "loan_repayment",
                                                                              "miscellaneous")))))))))

#keep only the month, category, subcategory, and amount columns
bank <- bank %>%
  select("month",
         "category",
         "subcategory",
         "amount")

#subset the data into pay, expenses, and investments
categories <- distinct(bank, category)
#categories$category

income <- bank %>%
  filter(category %in% c("income"))

investments <- bank %>%
  filter(category %in% c("investment"))

expenses <- bank %>%
  filter(!category %in% c("income",
                          "investment",
                          "loan_repayment"))

###########################################
#annual income

#calculate income by month
monthly_income <- income %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "income")

#keep only the monthly expenses in the last 12 months
last_12_income <- monthly_income %>% filter(month %in% rolling_months)

#calculate average monthly expenses
avg_monthly_income <- mean(last_12_income$monthly_amount)

#calculate annual estimated BASE income
est_annual_base_income <- round(avg_monthly_income *12)

#add BAH back in because we currently live in housing
est_annual_bah_income <- est_annual_base_income + (bah*12)

#add tsp contributions back in to give total
est_annual_income <- est_annual_bah_income + tsp

###########################################
#annual expenses

#calculate expenses by month
monthly_expenses <- expenses %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount) * -1) %>%
  mutate(type = "expense")

#keep only the monthly expenses in the last 12 months
last_12_exp <- monthly_expenses %>% filter(month %in% rolling_months)

#calculate average monthly expenses
avg_monthly_expenses <- mean(last_12_exp$monthly_amount)

#calculate adjusted monthly expenses
adj_monthly_expenses <- avg_monthly_expenses + housing_buffer

#estimate annual expenses
est_annual_expenses <- adj_monthly_expenses *12

###########################################
#calculate annual expense multiplier for FI calculations

#calculate expenses multiplier
expense_multiplyer <- 1/annual_safe_withdrawal

###########################################
#calculate annual investment percentage of income

#calculate investments by month
monthly_investments <- investments %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount) * -1) %>%
  mutate(type = "investment")

#keep only the monthly expenses in the last 12 months
last_12_invest <- monthly_investments %>% filter(month %in% rolling_months)

#total investments minus tsp
total_invest_less_tsp <- sum(last_12_invest$monthly_amount)

#add tsp
total_annual_invest <- total_invest_less_tsp + tsp

#calculate investments as a percent of annual pay
perc_inv_of_pay <- total_annual_invest/est_annual_income

###########################################
#calculate current value of investment accounts

#convert date field from character to date
net_worth <- mutate(net_worth, Date = as.Date(Date,format = "%m/%d/%y"))

#pivot from wide to long
net_worth_long <- pivot_longer(net_worth,!Date, names_to = "Fund_Type", values_to = "Value")

#keep only the funds of interest
investment_accounts <- filter(net_worth_long, Fund_Type %in% c("TSP",
                                                               "Vanguard_IRA_Jeff",
                                                               "Vanguard_IRA_Elaina",
                                                               "Vanguard_Brokerage"))

#calculate current value of investement accounts
inv_value <- investment_accounts %>%
  filter(Date == max(Date)) %>%
  summarise(investment_value = sum(Value))

investment_value <- inv_value[[1]]

###########################################
#calculate average annual returns

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$annual_return))/100

###########################################
#estimated annual pension

#determine next rank
if (c_rank == "O1"){
  n_rank <- "O2"
} else if (c_rank == "O2"){
  n_rank <- "O3"
} else if (c_rank == "O3"){
  n_rank <- "O4"
} else if (c_rank == "O4"){
  n_rank <- "O5"
} else if (c_rank == "O5"){
  n_rank <- "O6"
} else if (c_rank == "O6"){
  n_rank <- "O7"
} else if (c_rank == "O7"){
  n_rank <- "O8"
} else if (c_rank == "O8"){
  n_rank <- "O9"
} else if (c_rank == "O9"){
  n_rank <- "O10"
} else if (c_rank == "O1E"){
  n_rank <- "O2E"
} else if (c_rank == "O2E"){
  n_rank <- "O3E"
} else if (c_rank == "O3E"){
  n_rank <- "O4"
} 

n_rank

#calculate current years of service
c_yos <- time_length(difftime(Sys.Date(),pebd),"years")

#calculate years of experience for the pay scale
calculate_yoe(c_yos) 

# determine highest 36 months of pay

# create vector of 36 months based on estimated retirement date
final_36_months <- seq(as.Date(est_retire_date)%m-% months(36),as.Date(est_retire_date), by = "month")

#calculate years of service based on 36 month vector
est_yos <- round(time_length(difftime(final_36_months,pebd),"years"), digits = 2)

#calculate pension percentage from estimated years of service
est_pensions_percentage <- round(est_yos * 0.025, digits = 2)

#estimate rank, years of experience, and monthly base pay for the 36 months based on current 
#rank and estimated promotion date
est_rank <- seq(1:36)
est_yoe <- seq(1:36)
est_monthly_pay <- seq(1:36)

for (i in 1:36) {
  if (final_36_months[i] < est_promotion) {
    est_rank[i] = c_rank
  } else if (final_36_months[i] >= est_promotion) {
    est_rank[i] = n_rank
  }
  
  est_yoe[i] <- calculate_yoe(est_yos[i])
  
  if (year(final_36_months[i]) > 2022) {
    t_pc <- filter(pChart,YOE == est_yoe[i])
    est_monthly_pay[i] <- t_pc[,est_rank[i]][[1]]
  }
}

# estimate gross monthly pension
est_monthly_pension <- mean(est_monthly_pay) * max(est_pensions_percentage)

#calculate gross annual pension
est_annual_pension <- round(est_monthly_pension * 12)

#calculate net annual pension
est_annual_net_pension <- round(calculate_net_income(est_annual_pension))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#calculate Naive FI

#calculate Naive FI number
naive_fi <- est_annual_expenses * expense_multiplyer

#calculate years to FI based on current % invested
naive_fi_count_current <- 0
sim_invest_value <- investment_value
perc_invest = perc_inv_of_pay

while (sim_invest_value < naive_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) +
    (perc_invest*est_annual_income)
  naive_fi_count_current <- naive_fi_count_current + 1
}

naive_fi_count_current


#build the "simple math" chart from Mr. Money Mustache
inv_perc <- seq(0,20,by = 1)
years <- seq(0,20,by = 1)

for (i in 0:20) {
  naive_fi_count <- 0
  sim_invest_value <- investment_value
  inv_perc[i+1] <- i*5/100
  
  while (sim_invest_value < naive_fi) {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns) +
      ((i*5/100)*est_annual_income)
    naive_fi_count <- naive_fi_count + 1
  }
  
  years[i+1] <- naive_fi_count
}

#combine the sim data
naive_sim <- as_tibble(cbind(inv_perc,years))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#calculate Pension FI

#calculate Pension FI number
pension_fi <- (est_annual_expenses - est_annual_net_pension) * expense_multiplyer

#calculate years to FI based on current % invested
pension_fi_count_current <- 0
sim_invest_value <- investment_value
perc_invest = perc_inv_of_pay

while (sim_invest_value < pension_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) +
    (perc_invest*est_annual_income)
  pension_fi_count_current <- pension_fi_count_current + 1
}

pension_fi_count_current


#build the "simple math" chart from Mr. Money Mustache
inv_perc <- seq(0,20,by = 1)
years <- seq(0,20,by = 1)

for (i in 0:20) {
  pension_fi_count <- 0
  sim_invest_value <- investment_value
  inv_perc[i+1] <- i*5/100
  
  while (sim_invest_value < pension_fi) {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns) +
      ((i*5/100)*est_annual_income)
    pension_fi_count <- pension_fi_count + 1
  }
  
  years[i+1] <- pension_fi_count
}

#combine the sim data
pension_sim <- as_tibble(cbind(inv_perc,years))

#plot it
ggplot() +
  geom_col(data = naive_sim,
           mapping = aes(x = inv_perc,
                         y = years),
           fill = 'orange2') +
  geom_col(data = pension_sim,
           mapping = aes(x = inv_perc,
                         y = years),
           fill = 'blue4') +
  ylim(0,35) +
  geom_hline(yintercept = pension_fi_count_current,
             color = 'blue',
             linewidth = 2) +
  geom_hline(yintercept = naive_fi_count_current,
             color = 'orange',
             linewidth = 2) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Years to FI based on % of Income Invested (2023 Income)",
       subtitle = paste0("Orange = Naive; Blue = Pension;\n Horizontal lines show years to FI based on ",
                         scales::percent(perc_inv_of_pay),
                         " of income invested"),
       x = "Percent of Net Income Invested",
       y = "Years to Financial Independence")