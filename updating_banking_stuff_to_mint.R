
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

#read in mint transactions
bank <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_transactions.csv") %>%
  select(!c(Labels,Notes))

#read in mint categories
cats <- read_excel("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/mint_categories.xlsx",
                   sheet = "reference")

#create a 12 month list
rolling_12_months <- format(seq(lubridate::floor_date(today(), unit = "month") %m-% months(12),
                                lubridate::floor_date(today(), unit = "month") %m-% months(1),
                                by = "month"),
                            "%Y-%m")

#get rid of spaces in names
names(bank) <- sub(" ","_",names(bank))

#change all names to lower
names(bank) <- tolower(names(bank))

# do the same to category and account_name
unique(bank$category)
bank$category <- gsub("& ","",bank$category)
bank$category <- gsub(" ","_",bank$category)
bank$category <- tolower(bank$category)

unique(bank$account_name)
bank$account_name <- gsub(" ","_",bank$account_name)
bank$account_name <- tolower(bank$account_name)
bank$account_name <- gsub("_-_uniformed_services","",bank$account_name)

#change dates to date type
bank$date <- as.Date(mdy(bank$date))

#add HOF category
bank <- bank %>%
  left_join(cats, by = "category")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Calculate average expenses for last 12 months
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
expenses <- bank %>% filter(transaction_type == "debit",
                          !hof_category %in% c("transfer","investments"))

#change the date to month format
expenses$date <- format(expenses$date, "%Y-%m")

#keep only the last 12 months worth of expenses
expenses_last_12 <- expenses %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
expenses_last_12_grouped <- expenses_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_expenses <- round(mean(expenses_last_12_grouped$monthly_amount), digits = 2)

#calculate average annual expenses (minus housing)
avg_annual_expenses <- avg_monthly_expenses * 12
scales::dollar(avg_annual_expenses)

#plot it
ggplot(data = expenses_last_12_grouped,
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

income <- bank %>%
  filter(transaction_type == "credit",
         hof_category %in% c("income"))

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

#calculate average annual income (does not include BAH)
avg_annual_income <- avg_monthly_income * 12
scales::dollar(avg_annual_income)

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
bank %>% filter(transaction_type == "credit",
                hof_category == "investments") %>%
  group_by(account_name) %>% summarise(total = sum(amount))

investments <- bank %>% 
  filter(hof_category == "investments",
         #account_name %in% c("jeff_and_elaina_brokerage","thrift_savings_plan"),
         !category %in% c("dividend_cap_gains"),
         transaction_type == "credit",
         !grepl("Settlement",description)) %>%
  mutate(type = if_else(account_name %in% c("thrift_savings_plan","jeff_ira","elaina_ira"),
                        "retirement",
                        "non-retirement"))

investments %>%
  group_by(account_name) %>% summarise(total = sum(amount))

#change the date to month format
investments$date <- format(investments$date, "%Y-%m")

#keep only the last 12 months worth of expenses
investments_last_12 <- investments %>%
  filter(date %in% rolling_12_months)

#calculate average monthly expenses for last 12 months
investments_last_12_grouped_type <- investments_last_12 %>%
  group_by(date, type) %>%
  summarise(monthly_amount = sum(amount))

investments_last_12_grouped <- investments_last_12 %>%
  group_by(date) %>%
  summarise(monthly_amount = sum(amount))

avg_monthly_investments <- round(mean(investments_last_12_grouped$monthly_amount), digits = 2)

#plot it
ggplot(data = investments_last_12_grouped_type) +
  geom_hline(aes(yintercept = avg_monthly_investments)) +
  geom_col(aes(x = date, y = monthly_amount, fill = type)) +
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
                                 linetype = "solid"),
        legend.position = "bottom")
