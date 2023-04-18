library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

#read in table of historical S&P 500 returns
sp_500 <- read_csv("/Users/jeffgood/Desktop/FI/sp-500-historical-annual-returns.csv")

#calculate average annual return for S&P 500
avg_annual_returns <- floor(mean(sp_500$Annual_Return))/100

###############################################################################
#calculate current value of investment accounts

#read in net worth document
net_worth <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")

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

################################################################################
# calculate current rolling 12 months expenses

#read expenses file in 
bank <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/Expenses.xlsx",
                   sheet = "Transactions")

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
#bank %>% distinct(subcategory)
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

#calculate expenses by month
monthly_expenses <- expenses %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount) * -1) %>%
  mutate(type = "expense")

#create a 12 month list
rolling_months <- format(seq(today()-365, today() %m-% months(1), by = 'month'), "%Y-%m")

#keep only the monthly expenses in the last 12 months
last_12_exp <- monthly_expenses %>% filter(month %in% rolling_months)

#calculate average monthly expenses
avg_monthly_expenses <- mean(last_12_exp$monthly_amount)

#add a housing buffer
housing_buffer <- 3000

#calculate adjusted monthly expenses
adj_monthly_expenses <- avg_monthly_expenses + housing_buffer

#estimate annual expenses
est_annual_expenses <- adj_monthly_expenses *12

################################################################################
#calculate naive FI requirements

#set annual safe withdrawal rate
annual_safe_withdrawal <- 0.03

#calculate expenses multiplier
expense_multiplyer <- 1/annual_safe_withdrawal

#naive FI number
naive_fi <- est_annual_expenses * expense_multiplyer

################################################################################
# Calculate years to naive FI assuming:
# no additional contributions to investment value

naive_fi_count <- 0
sim_invest_value <- investment_value

while (sim_invest_value < naive_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns)
  naive_fi_count <- naive_fi_count + 1
}

naive_fi_count

################################################################################
# Calculate years to naive FI assuming:
# max TSP contributions

#set max TSP contributions
max_tsp <- 20500

naive_fi_tsp_count <- 0
sim_invest_value <- investment_value
while (sim_invest_value < naive_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) +
    max_tsp
  naive_fi_tsp_count <- naive_fi_tsp_count + 1
}

naive_fi_tsp_count

################################################################################
# Calculate years to naive FI assuming:
# max TSP contributions and
# investing difference between average monthly expenses and income

#calculate expenses by month
monthly_income <- income %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "income")

#keep only the monthly expenses in the last 12 months
last_12_income <- monthly_income %>% filter(month %in% rolling_months)

#calculate average monthly expenses
avg_monthly_income <- mean(last_12_income$monthly_amount)

#set BAH amount
bah <- 2600

#calculate estimate annual income and add BAH back in
est_annual_income <- (avg_monthly_income + bah) *12

#calculate the delta between income and expenses monthly
annual_income_delta <- est_annual_income - est_annual_expenses

#calucate the percent of annual income
perc_income <- round(annual_income_delta/est_annual_income, digits = 2)

#calculate years to FI
naive_fi_tsp_inc_count <- 0
sim_invest_value <- investment_value

while (sim_invest_value < naive_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns) +
    max_tsp + 
    (est_annual_income*perc_income)
  naive_fi_tsp_inc_count <- naive_fi_tsp_inc_count + 1
}

naive_fi_tsp_inc_count


################################################################################
# rerun calculations based on estimate pension

################################################################################
#estimate annual pension

# load data
pChart <- read_csv("/Users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/2023_Officer_Pay.csv")
#pChart

# personal information
pebd <- as.Date(mdy("2-19-1999")) #pay entry based date

c_rank <- "O4" #current rank

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

c_dor <- as.Date(mdy("9-1-2018")) #current date of rank

est_promotion <- as.Date(mdy("9-1-2023")) #expected promotion date

est_retire_date <- as.Date(mdy("09-1-2026")) #expected retirement date

c_yos <- time_length(difftime(Sys.Date(),pebd),"years")  #current years of service (yos)

# create a function to calculate years of experience (yoe)

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
est_annual_pension <- est_monthly_pension * 12

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

#calculate net annual pension
est_annual_net_pension <- calculate_net_income(est_annual_pension)

################################################################################
# Calculate a new FI number based on estimated gross pension

pension_fi <- (est_annual_expenses - est_annual_net_pension) * expense_multiplyer

################################################################################
# Calculate years to pension FI assuming:
# no additional contributions to investment value

pension_fi_count <- 0
sim_invest_value <- investment_value

while (sim_invest_value < pension_fi) {
  sim_invest_value = sim_invest_value +
    (sim_invest_value*avg_annual_returns)
  pension_fi_count <- pension_fi_count + 1
}

pension_fi_count

################################################################################
# Calculate years to naive FI assuming:
# max TSP contributions until Navy retirement

#calculate years until Navy retirement
years_to_retire <- floor(interval(today(),est_retire_date) / years(1))

pension_fi_tsp_count <- 0
sim_invest_value <- investment_value

while (sim_invest_value < pension_fi) {
  if (pension_fi_tsp_count <= years_to_retire) {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns) +
      max_tsp
    
    pension_fi_tsp_count <- pension_fi_tsp_count + 1
  } else {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns)
    
    pension_fi_tsp_count <- pension_fi_tsp_count + 1
  }
}

pension_fi_tsp_count

################################################################################
# Calculate years to pension FI assuming:
# max TSP contributions and
# investing difference between average monthly expenses and income until retirement

pension_fi_tsp_inc_count <- 0
sim_invest_value <- investment_value

while (sim_invest_value < pension_fi) {
  if (pension_fi_tsp_inc_count <= years_to_retire) {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns) +
      max_tsp + 
      (est_annual_income*perc_income)
    
    pension_fi_tsp_inc_count <- pension_fi_tsp_inc_count + 1
  } else {
    sim_invest_value = sim_invest_value +
      (sim_invest_value*avg_annual_returns)
    
    pension_fi_tsp_inc_count <- pension_fi_tsp_inc_count + 1
  }
}

pension_fi_tsp_inc_count


###############################################################################
# Run some estimates on post navy retirement pay

# assume elaina's pay based on cy 2022 pay for Spring Heath (2 months)

cy22_pay <- 6500

est_monthly_pay_elaina <- cy22_pay/2

est_annual_pay_elaina <- est_monthly_pay_elaina *12

#estimate family's annual gross income based on est elaina pay and gross pension
est_annual_gross_family_income <- round(est_annual_pension + est_annual_pay_elaina)

#calculate estimated net family income
est_annual_net_family_income <- calculate_net_income(est_annual_gross_family_income)

#calculate the delta between estimated family income and expenses
est_annual_net_family_income-est_annual_expenses
