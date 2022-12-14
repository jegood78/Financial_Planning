#libraries
library(tidyverse)
library(lubridate)
library(readxl)

#read in files
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
                                                                     "miscellaneous")))))))


#keep only the month, category, subcategory, and amount columns
bank <- bank %>%
  select("month",
         "category",
         "subcategory",
         "amount")

#group by month
monthly_by_category <- bank %>%
  group_by(month, category) %>%
  summarise(monthly_amount = sum(amount))

#initial plot
ggplot(data = monthly_by_category,
       mapping = aes(x = category,
                     y = monthly_amount,
                     fill = category)) +
  geom_col() +
  facet_grid(~month)

#subset the data into pay, expenses, and investments
categories <- distinct(bank, category)
categories$category

income <- bank %>%
  filter(category %in% c("income"))

investments <- bank %>%
  filter(category %in% c("investment"))

expenses <- bank %>%
  filter(!category %in% c("income",
                          "investment"))

#plot expenses by month
monthly_expenses <- expenses %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount) * -1) %>%
  mutate(type = "expense")

ggplot() +
  geom_col(data = monthly_expenses,
           aes(x = month, y = monthly_amount),
           fill = "red") +
  theme_minimal() +
  geom_text(data = monthly_expenses,
            aes(x = month,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  labs(title = "Expenses by Month", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

monthly_expenses_by_type <- expenses %>%
  group_by(month, category) %>%
  summarise(monthly_amount = sum(amount) * -1) %>%
  arrange(desc(monthly_amount))

ggplot(data = monthly_expenses_by_type,
       aes(x = reorder(category, -desc(monthly_amount)),
           y = monthly_amount,
           fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~month) +
  geom_text(aes(y = 2,
                label = paste0(category, " $", monthly_amount),
                group = category),
            hjust = 0) +
  coord_flip() +
  ggtitle("Monthly Expenses by Category") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#plot income by month
monthly_income <- income %>%
  group_by(month) %>%
  summarise(monthly_amount = sum(amount)) %>%
  mutate(type = "income")

ggplot() +
  geom_col(data = monthly_income,
           aes(x = month, y = monthly_amount),
           fill = "green") +
  theme_minimal() +
  geom_text(data = monthly_income,
            aes(x = month,
                y = monthly_amount,
                label = paste0("$", monthly_amount)),
            vjust = -0.5) +
  labs(title = "Income by Month", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey",
                                 linewidth = 1,
                                 linetype = "solid"))

#combine the plots
combined <- rbind(monthly_expenses, monthly_income)

ggplot(data = combined,
       aes(x = type, y = monthly_amount, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(~month) +
  geom_text(aes(x = type,
                y = 2,
                label = paste0(toupper(type), "\n$", monthly_amount),
                group = type),
            hjust = 0) +
  coord_flip() +
  labs(title = "Cash Flow Summary by Month", x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
