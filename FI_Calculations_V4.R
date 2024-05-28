
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

#keep only the accounts we care about
unique(bank$account)

bank_redux <- bank %>%
  filter(account %in% c("signature_visa",
                        "usaa_savings",
                        "usaa_checking",
                        "thrift_savings_plan"),
         !category %in% c("eleanor_savings",
                          "adeline_savings",
                          "usaa_credit_card_payment")) %>%
  mutate(category = if_else(account %in% c("thrift_savings_plan"),
                            "investment",
                            category))

#assign to budget group (fixed expenses, investments, savings, misc, income)
unique(bank_redux$category)

bank_redux1 <- bank_redux %>%
  mutate(budget_group = if_else(category %in% c("jeff_pay",
                                                "elaina_pay",
                                                "other_income",
                                                "interest",
                                                "paychecks_salary"),
                                "income",
                                if_else(category %in% c("rent",
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
                                                        "internet"),
                                        "fixed_expenses",
                                        if_else(category %in% c("savings"),
                                                "savings",
                                                if_else(category %in% c("investment"),
                                                        "investment",
                                                        "misc_spending"))))) 

#summarize by month
#change the date to month format
bank_redux1$date <- format(ymd(bank_redux1$date), "%Y-%m")

income <- bank_redux1 %>%
  filter(budget_group %in% c("income")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "in",
         budget_group = "income")

fixed_expenses <- bank_redux1 %>%
  filter(budget_group %in% c("fixed_expenses")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)*-1) %>%
  mutate(type = "out",
         budget_group = "fixed_expenses")

savings <- bank_redux1 %>%
  filter(account %in% c("usaa_savings"),
         budget_group %in% c("savings")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "out",
         budget_group = "savings")

investment <- bank_redux1 %>%
  filter(budget_group %in% c("investment")) %>%
  mutate(amount = if_else(amount < 0,
                          amount*-1,
                          amount)) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "out",
         budget_group = "investment")

misc_spending <- bank_redux1 %>%
  filter(budget_group %in% c("misc_spending")) %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount)*-1) %>%
  mutate(type = "out",
         budget_group = "misc_spending")

monthly_merged <- rbind(income,
                        fixed_expenses,
                        savings,
                        investment,
                        misc_spending)
