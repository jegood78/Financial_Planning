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

#add month column
bank$month <- format(bank$date,"%Y-%m")

#subset the data into pay, expenses, and investments
categories <- distinct(bank,category)
categories$category

income <- bank %>% filter(category %in% c("Jeff_Pay","Miscellaneous_Income"))

investments <- bank %>% filter(category %in% c("Investment_Account"))

expenses <- bank %>% 
  filter(!category %in% c("Jeff_Pay","Miscellaneous_Income","Investment_Account")) 

#plot expenses by month
monthly_expenses <- expenses %>% 
  group_by(month) %>% 
  summarise(monthly_amount = sum(amount)*-1) %>% 
  mutate(type = "expense")

ggplot() +
  geom_col(data = monthly_expenses,aes(x=month,y=monthly_amount), fill = "red") +
  theme_minimal() +
  geom_text(data = monthly_expenses,aes(x=month,y=monthly_amount, label = paste0("$",monthly_amount)), vjust = -0.5) +
  labs(title = "Expenses by Month", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"))

monthly_expenses_by_type <- expenses %>%
  group_by(month,category) %>%
  summarise(monthly_amount = sum(amount)*-1)

# ggplot(aes(x=`Person of Interest`,
#            y=Proportion,
#            fill=`Person of Interest`,
#            label = paste0(`Person of Interest`, "~", scales::percent(Proportion))))+

ggplot(data = monthly_expenses_by_type, aes(x=category,
                                            y=monthly_amount,
                                            fill = category,
                                            label = paste0(category,"~$",monthly_amount))) +
  geom_bar(stat = "identity") +
  facet_wrap(~month) +
  geom_text(aes(y=2, 
                label = paste0(category,"~$",monthly_amount),
                group = category),
            hjust = 0) + coord_flip()+
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
  summarise(monthly_amount = sum(amount)) %>% mutate(type = "income")

ggplot() +
  geom_col(data = monthly_income,aes(x=month,y=monthly_amount), fill = "green") +
  theme_minimal() +
  geom_text(data = monthly_income,aes(x=month,y=monthly_amount, label = paste0("$",monthly_amount)), vjust = -0.5) +
  labs(title = "Expenses by Month", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"))

#combine the plots
combined <- rbind(monthly_expenses,monthly_income)

ggplot(data= combined,aes(x=type, y=monthly_amount, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(~month) +
  geom_text(aes(x=type,
                y=monthly_amount, 
                label = paste0(toupper(type),"\n$",monthly_amount),
                group = type), 
            vjust = 0.5,
            position=position_dodge2(width=0.9)) +
  labs(title = "Cash Flow Summary by Month", x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
