
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
#read in bank transactions from empower
#historical transactions
bank <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/empower_transactions_raw.csv")
bank

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

#tsp monthly
tsp_pre_032024 <- 2285
tsp_post_032024 <- 1942

#keep only the accounts we care about
unique(bank$account)

bank_redux <- bank %>%
  filter(account %in% c("signature_visa",
                        "usaa_savings",
                        "usaa_checking"),
         !category %in% c("eleanor_savings",
                          "adeline_savings",
                          "usaa_credit_card_payment")) 

#assign to budget group (fixed expenses, investments, savings, misc, income)
unique(bank_redux$category)

income <- bank_redux %>%
  filter(category %in% c("jeff_pay",
                         "elaina_pay",
                         "other_income",
                         "interest",
                         "paychecks_salary")) %>%
  mutate(date = format(ymd(date), "%Y-%m")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "in",
         budget_group = "income",
         monthly_amount = if_else(date < "2024-03",
                                  monthly_amount + tsp_pre_032024,
                                  monthly_amount + tsp_post_032024))

fixed_expenses <- bank_redux %>%
  filter(category %in% c("rent",
                         "vehicle_loan",
                         "groceries",
                         "school_loan",
                         "telephone",
                         "gym",
                         "pets_pet_care",
                         "automotive",
                         "gasoline_fuel",
                         "diapers_babyfood",
                         "insurance",
                         "taxes",
                         "loans",
                         "education",
                         "dues_subscriptions",
                         "cable_satellite",
                         "internet",
                         "healthcare_medical")) %>%
  mutate(date = format(ymd(date), "%Y-%m")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)*-1) %>%
  mutate(type = "out",
         budget_group = "fixed_expenses")

savings <- bank_redux %>%
  filter(account %in% c("usaa_savings"),
         category %in% c("savings")) %>%
  mutate(date = format(ymd(date), "%Y-%m")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "out",
         budget_group = "savings")

investment <- bank_redux %>%
  filter(grepl("invest", category)) %>%
  mutate(date = format(ymd(date), "%Y-%m")) %>%
  mutate(amount = if_else(amount < 0,
                          amount*-1,
                          amount)) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "out",
         budget_group = "investment",
         monthly_amount = if_else(date < "2024-03",
                                  monthly_amount + tsp_pre_032024,
                                  monthly_amount + tsp_post_032024))

misc_spending <- bank_redux %>%
  filter(!category %in% c("jeff_pay",
                          "elaina_pay",
                          "other_income",
                          "interest",
                          "paychecks_salary",
                          "rent",
                          "vehicle_loan",
                          "groceries",
                          "school_loan",
                          "telephone",
                          "gym",
                          "pets_pet_care",
                          "automotive",
                          "gasoline_fuel",
                          "diapers_babyfood",
                          "insurance",
                          "taxes",
                          "loans",
                          "education",
                          "dues_subscriptions",
                          "cable_satellite",
                          "internet",
                          "healthcare_medical",
                          "savings",
                          "investment")) %>%
  mutate(date = format(ymd(date), "%Y-%m")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)*-1) %>%
  mutate(type = "out",
         budget_group = "misc_spending")

monthly_merged_long <- rbind(income,
                        fixed_expenses,
                        savings,
                        investment,
                        misc_spending)

monthly_merged_wide <- income %>% 
  select(date, monthly_amount) %>% 
  rename("income_monthly" = "monthly_amount") %>%
  left_join(fixed_expenses %>%
              select(date, monthly_amount) %>%
              rename("fixed_expenses_monthly" = "monthly_amount")) %>%
  mutate(fixed_expenses_percent = scales::percent(fixed_expenses_monthly/income_monthly)) %>%
  left_join(savings %>%
              select(date, monthly_amount) %>%
              rename("savings_monthly" = "monthly_amount")) %>%
  mutate(savings_monthly = if_else(is.na(savings_monthly),0,savings_monthly),
         savings_percent = scales::percent(savings_monthly/income_monthly)) %>%
  left_join(investment %>%
              select(date, monthly_amount) %>%
              rename("investment_monthly" = "monthly_amount")) %>%
  mutate(investment_monthly = if_else(is.na(investment_monthly),0,investment_monthly),
         investment_percent = scales::percent(investment_monthly/income_monthly)) %>%
  left_join(misc_spending %>%
              select(date, monthly_amount) %>%
              rename("misc_spending_monthly" = "monthly_amount")) %>%
  mutate(misc_spending_percent = scales::percent(misc_spending_monthly/income_monthly))
