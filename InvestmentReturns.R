# load libraries
library(tidyverse)
library(lubridate)

#read in table of historical S&P 500 returns

sp_500 <- read_csv("/Users/jeffgood/Desktop/R_Studio_Projects/Financial_Planning/sp-500-historical-annual-returns.csv")
sp_500

hist(sp_500$Annual_Return)

#build distribution of historical S&P 500 returns in order to sample 
bottom <- round(min(sp_500$Annual_Return))-1
#bottom <- floor(min(sp_500$Annual_Return)*10)/10

top <- round(max(sp_500$Annual_Return))
#top <- ceiling(max(sp_500$Annual_Return)*10)/10

sp_500 <- sp_500 %>% 
  mutate(new_bin = cut(Annual_Return, breaks = seq(bottom,top,1), labels = seq(bottom+1,top,1)))

sp_500$new_bin

bin_counts <- sp_500 %>% 
  group_by(new_bin) %>% 
  summarise(return_count = n())

bin_counts <- bin_counts %>% 
  mutate(count_percent = return_count/sum(return_count))

bin_counts <- bin_counts %>% 
  mutate(cum_percent = cumsum(count_percent))

for (i in 1:dim(bin_counts)[1]){
  if (i==1) {
    bin_counts$left[i] <- 0.000
  } else if (i > 1) {
    bin_counts$left[i] <- round(bin_counts[i-1,"cum_percent"][[1]],4) + 0.0001
  }
  if (i==dim(bin_counts)[1]) {
    bin_counts$right[i] <- 1.000
  } else {
    bin_counts$right[i] <- round(bin_counts[i,"cum_percent"][[1]],4)
  }
}

return_dist <- bin_counts %>% select(c("left","right","new_bin")) %>% rename("return" = "new_bin")

#calculate the simulated return from the historical distribution
rand_return <- function(x) {
  for (i in 1:dim(return_dist)[1]) {
    if (x >= return_dist$left[i] & x <= return_dist$right[i]) {
      return(as.numeric(as.character(return_dist$return[i]))/100)
    }
  }
}

rand_return(runif(1))

#run a single simulation (no contributions/no withdrawals)
num_years <- 46

net_worth_start <- 475000

years <- seq(1,num_years,1)
rtn_input <- runif(num_years)
rtn <- seq(1,num_years,1)
net_worth <- seq(1,num_years,1)

for (i in 1:num_years) {
  if (i == 1) {
    years[i] <- year(Sys.Date())
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- net_worth_start
  } else {
    years[i] <- years[i-1]+1
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- net_worth[i-1]*(1+rtn[i])
  }
}

net_worth


#run a single simulation (one set of contributions/no withdrawals)
num_years <- 46

net_worth_start <- 475000

annual_contr1 <- 28000

contr1_years <- 4

years <- seq(1,num_years,1)
contr1 <- seq(1,num_years,1)
rtn_input <- runif(num_years)
rtn <- seq(1,num_years,1)
net_worth <- seq(1,num_years,1)

for (i in 1:num_years){
  if (i <= contr1_years) {
    contr1[i] <- annual_contr1
  } else {
    contr1[i] <- 0
  }
}

for (i in 1:num_years) {
  if (i == 1) {
    years[i] <- year(Sys.Date())
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- net_worth_start
  } else {
    years[i] <- years[i-1]+1
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- (net_worth[i-1] + contr1[i])*(1+rtn[i])
  }
}

net_worth


#run a single simulation (two contributions/no withdrawals)
num_years <- 46

net_worth_start <- 475000

annual_contr1 <- 28000

contr1_years <- 4

annual_contr2 <- 80000

contr2_years <- 5

years <- seq(1,num_years,1)
contributions <- seq(1,num_years,1)
rtn_input <- runif(num_years)
rtn <- seq(1,num_years,1)
net_worth <- seq(1,num_years,1)

for (i in 1:num_years){
  if (i <= contr1_years) {
    contributions[i] <- annual_contr1
  } else if (i > contr1_years & i <= (contr1_years + contr2_years)){
    contributions[i] <- annual_contr2
  } else {
    contributions[i] <- 0
  }
}

for (i in 1:num_years) {
  if (i == 1) {
    years[i] <- year(Sys.Date())
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- net_worth_start
  } else {
    years[i] <- years[i-1]+1
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- (net_worth[i-1] + contributions[i])*(1+rtn[i])
  }
}

net_worth


#run a single simulation (two contributions/with withdrawals)
num_years <- 46

net_worth_start <- 475000

annual_contr1 <- 28000

contr1_years <- 4

annual_contr2 <- 80000

contr2_years <- 5

withdrawal_start_year <- 2031

withdrawal_anount <- 30000

years <- seq(1,num_years,1)
contributions <- seq(1,num_years,1)
withdrawals <- seq(1,num_years,1)
rtn_input <- runif(num_years)
rtn <- seq(1,num_years,1)
net_worth <- seq(1,num_years,1)

for (i in 1:num_years){
  if (i <= contr1_years) {
    contributions[i] <- annual_contr1
  } else if (i > contr1_years & i <= (contr1_years + contr2_years)){
    contributions[i] <- annual_contr2
  } else {
    contributions[i] <- 0
  }
}

for (i in 1:num_years) {
  if (i == 1) {
    years[i] <- year(Sys.Date())
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- net_worth_start
    withdrawals[i] <- 0
  } else {
    years[i] <- years[i-1]+1
    if (years[i] >= withdrawal_start_year) {
      withdrawals[i] <- 30000
    } else {
      withdrawals[i] <- 0
    }
    rtn[i] <- rand_return(rtn_input[i])
    net_worth[i] <- (net_worth[i-1] - withdrawals[i] + contributions[i])*(1+rtn[i])
  }
}

withdrawals
net_worth

