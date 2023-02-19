#convert date field from character to date
net_worth <- mutate(net_worth, Date = as.Date(Date,format = "%m/%d/%y"))

#pivot from wide to long
net_worth_long <- pivot_longer(net_worth,!Date, names_to = "Fund_Type", values_to = "Value")

#keep only the funds of interest
investment_accounts <- filter(net_worth_long, Fund_Type %in% c("TSP",
                                                          "Vanguard_IRA_Jeff",
                                                          "Vanguard_IRA_Elaina",
                                                          "Vanguard_Brokerage"))

#convert the fund types to factors
investment_accounts <- investment_accounts %>%
  mutate(Fund_Type = fct_relevel(Fund_Type))

#list of retirement funds
retirement_funds <- c("TSP",
                      "Vanguard_IRA_Jeff",
                      "Vanguard_IRA_Elaina")

#list of nonretirement funds
nonretirement_funds <- c("Vanguard_Brokerage",
                         "USAA_Savings",
                         "USAA_Checking",
                         "USAA_Visa",
                         "Chase_CC",
                         "School_Loan_Elaina")

#sum the retirement funds accounts for each entry
net_worth$Retirement_Funds <- rowSums(nw[,retirement_funds])

#sum the nonretirement fund accounts for each entry
net_worth$Nonretirement_Funds <- rowSums(nw[,nonretirement_funds])

#calculate total net worth
net_worth$Net_Worth <- rowSums(nw[,c("Retirement_Funds","Nonretirement_Funds")])

#keep only the date and calculated fields
net_worth_sums <- net_worth[,c("Date","Retirement_Funds","Nonretirement_Funds","Net_Worth")]

#pivot the calculated fields longer
net_worth_sums_long <- pivot_longer(net_worth_sums,!Date, names_to = "Fund_Type", values_to = "Total_Value")

#change fund_type to factor and create a label on the max date
net_worth_sums_long <- net_worth_sums_long %>%
  mutate(Fund_Type = fct_relevel(Fund_Type),
         label = if_else(Date == max(Date),
                                      paste0("$",round(Total_Value/1000),"K"),
                                      NULL))

#calculate the max net worth
max_net_worth <- max(net_worth_sums_long$Total_Value)

#use max net worth to calculate a max value for the y access
y_max <- plyr::round_any(max_net_worth + 50000, 50000, f=ceiling)

#plot the net worth
ggplot(data = net_worth_sums_long,
       mapping= aes(x=Date,
                    y=Total_Value,
                    color=Fund_Type,
                    label = label)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "grey", 
                                 linewidth = 1,
                                 linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = label),
             color = "black",
             nudge_y = 15000,
             size = 3) +
  expand_limits(y = c(0, y_max)) +
  scale_y_continuous(labels = scales::dollar,breaks = seq(0, y_upper_nw1, 50000)) +
  labs(title = "Good Family Net Worth", x = NULL, y = NULL)

#find the max value for the investment accounts
max_value <- max(investment_accounts$Value)

#create a value for to use for the y-axis
y_max2 <- plyr::round_any(max_value + 50000, 50000, f=ceiling)

#create a label on the max date
investment_accounts <- investment_accounts %>%
  mutate(label = if_else(Date == max(Date),
                                 paste0("$",round(Value/1000),"K"),
                                 NULL))

#plot the investment accounts
ggplot(data = investment_accounts,
       mapping = aes(x=Date,
                     y=Value,
                     color = Fund_Type,
                     label = label)) +
  #geom_col(position = "dodge") +
  geom_line() +
  geom_point() +
  #facet_grid(Fund_Type~) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "grey", 
                                 linewidth = 1,
                                 linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(color = "black",
            nudge_y = 10000,
            size = 3) +
  expand_limits(y = c(0, y_max2)) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, y_upper_nw0, 50000)) +
  labs(title = "Good Family Investment Accounts", x = NULL, y = NULL)
                