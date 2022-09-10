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
#   geom_bar(stat = 'identity')+
#   geom_text(aes(y=0.13),
#             size = 4.5,
#             colour = "white",
#             fontface = "bold")+coord_flip()+
#   scale_y_continuous(labels = scales::percent, 
#                      limits = c(0, 1.01), 
#                      expand = c(0, 0)) +
#   ggthemes::theme_economist(horizontal = F) + 
#   scale_fill_manual(values = alpha(c("black", "#002D62"), .5)) +
#   ggtitle("Lack of Skill") + 
#   theme(title = element_text("Lack of Skill"),
#         plot.title = element_text(hjust = 0.5, face = "italic"),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(hjust = 0.25),
#         legend.position="none",
#         aspect.ratio = 1/3)

ggplot(data = monthly_expenses_by_type) +
  geom_col(aes(x=monthly_amount,
               y=category, 
               fill = category)) +
  facet_wrap(~month) +
  theme_bw() +
  geom_text(aes(x=monthly_amount,
                y=category, 
                label = paste0("$",monthly_amount),
                group = category), 
            hjust = 0.6,
            position=position_dodge(width=0.9)) +
  theme(legend.position = "none",
        axis.text.y = element_text(vjust = 0.5, 
                                   hjust=1),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", 
                                 size = 1, 
                                 linetype = "solid"),
        axis.line.y = element_line(colour = "black", 
                                   size = 1,
                                   linetype = "solid")) 

        
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

ggplot(data= combined,aes(x=month,y=monthly_amount, fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  geom_text(aes(x=month,
                y=monthly_amount, 
                label = paste0("$",monthly_amount),
                group = type), 
            vjust = -0.5,
            position=position_dodge2(width=0.9)) +
  labs(title = "Cash Flow Summary by Month", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(face="bold"),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        legend.position = "bottom")
