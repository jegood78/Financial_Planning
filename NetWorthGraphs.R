library(tidyverse)
library(tidyr)
library(plyr)

nw <- read_csv("/users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/NetWorth.csv")#,col_select = "Date":"School_Loan_Elaina")

nw <- mutate(nw, Date = as.Date(Date,format = "%m/%d/%y"))

nw0 <- pivot_longer(nw,!Date, names_to = "Fund_Type", values_to = "Value")

nw0 <- filter(nw0, Fund_Type %in% c("TSP","Vanguard_IRA_Jeff","Vanguard_IRA_Elaina","Vanguard_Brokerage"))

nw0 <- nw0 %>% mutate(Fund_Type = fct_relevel(Fund_Type))

RCols <- c("TSP","Vanguard_IRA_Jeff","Vanguard_IRA_Elaina")

NRCols <- c("Vanguard_Brokerage","USAA_Savings","USAA_Checking",
            "USAA_Visa","Chase_CC","School_Loan_Elaina")

nw$Retirement_Funds <- rowSums(nw[,RCols])

nw$Nonretirement_Funds <- rowSums(nw[,NRCols])

nw$Net_Worth <- rowSums(nw[,c("Retirement_Funds","Nonretirement_Funds")])

nw1 <- nw[,c("Date","Retirement_Funds","Nonretirement_Funds","Net_Worth")]

nw1 <- pivot_longer(nw1,!Date, names_to = "Fund_Type", values_to = "Total_Value")

nw1 <- nw1 %>% mutate(Fund_Type = fct_relevel(Fund_Type))

top_nw1 <- max(nw1$Total_Value)
y_upper_nw1 <- plyr::round_any(top_nw1 + 50000,50000,f=ceiling)

ggplot(data = nw1,mapping= aes(x=Date,y=Total_Value,color=Fund_Type, label = scales::dollar(Total_Value))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "grey", 
                                 size = 1, linetype = "solid"),
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(color = "black", nudge_y = 15000, size = 3) +
  expand_limits(y = c(0,y_upper_nw1)) +
  scale_y_continuous(labels = scales::dollar,breaks = seq(0,y_upper_nw1,50000)) +
  labs(title = "Good Family Net Worth", x = NULL, y = NULL)

top_nw0 <- max(nw0$Value)
y_upper_nw0 <- plyr::round_any(top_nw0 + 50000,50000,f=ceiling)

ggplot(data = nw0, mapping = aes(x=Date, y=Value, color = Fund_Type, label = scales::dollar(Value))) +
  #geom_col(position = "dodge") +
  geom_line() +
  geom_point() +
  #facet_grid(Fund_Type~) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "grey", 
                                 size = 1, linetype = "solid"),
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(color = "black", nudge_y = 10000,size = 3) +
  expand_limits(y = c(0,y_upper_nw0)) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0,y_upper_nw0,50000)) +
  labs(title = "Good Family Investment Accounts", x = NULL, y = NULL)
                