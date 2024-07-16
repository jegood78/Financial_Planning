#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Load libraries
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(writexl)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# set constants
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
#tsp monthly
tsp_pre_032024 <- 2285
tsp_post_032024 <- 1942

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# banking files
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

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
bank %>% group_by(key) %>% summarise(count = n()) %>% arrange(desc(count))

#remove duplicates
bank <- distinct(bank)

#check duplicates by 
bank %>% group_by(key) %>% summarise(count = n()) %>% arrange(desc(count))

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

#create a visualization
ggplot(data = bank_monthly,
       aes(x = month,
           y = monthly_amount,
           group = flow,
           fill = transaction_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("fixed_expenses" = "red",
                               "misc_expenses" = "red4",
                               "savings" = "blue",
                               "investment" = "skyblue",
                               "income" = "green4")) +
  scale_y_continuous(seq(0,25000,500), labels = dollar)+
  theme_minimal() +
  labs(title = "Income and Expenses by Month", subtitle = "Last 12 Months", x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"),
        legend.position = "bottom")
