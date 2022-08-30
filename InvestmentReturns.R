# load libraries
library(tidyverse)
library(lubridate)
library(gsubfn) #to return more than one item from a function

#calculate the simulated return from the historical distribution

rand_return <- function(x){
  if (x>=0 & x<0.0107) {
    y <- -47
  } else if (x>=0.0107 & x<0.0320) {
    y <- -38
  } else if (x>=0.0320 & x<0.0427) {
    y <- -29
  } else if (x>=0.0427 & x<0.0533) {
    y <- -28
  } else if (x>=0.0533 & x<0.0639) {
    y <- -23
  } else if (x>=0.0639 & x<0.0852) {
    y <- -17
  } else if (x>=0.0852 & x<0.1065) {
    y <- -15
  } else if (x>=0.1065 & x<0.1171) {
    y <- -14
  } else if (x>=0.1171 & x<0.1384) {
    y <- -13
  } else if (x>=0.1384 & x<0.1916) {
    y <- -11
  } else if (x>=0.1916 & x<0.2022) {
    y <- -10
  } else if (x>=0.2022 & x<0.2129) {
    y <- -9
  } else if (x>=0.2129 & x<0.2448) {
    y <- -6
  } else if (x>=0.2448 & x<0.2661) {
    y <- -5
  } else if (x>=0.2661 & x<0.2767) {
    y <- -2
  } else if (x>=0.2767 & x<0.2873) {
    y <- -1
  } else if (x>=0.2873 & x<0.3299) {
    y <- 0
  } else if (x>=0.3299 & x<0.3405) {
    y <- 1
  } else if (x>=0.3405 & x<0.3618) {
    y <- 2
  } else if (x>=0.3618 & x<0.3937) {
    y <- 3
  } else if (x>=0.3937 & x<0.4044) {
    y <- 4
  } else if (x>=0.4044 & x<0.4150) {
    y <- 5
  } else if (x>=0.4150 & x<0.4363) {
    y <- 8
  } else if (x>=0.4363 & x<0.4575) {
    y <- 9
  } else if (x>=0.4575 & x<0.4788) {
    y <- 10
  } else if (x>=0.4788 & x<0.5001) {
    y <- 11
  } else if (x>=0.5001 & x<0.5214) {
    y <- 12
  } else if (x>=0.5214 & x<0.5746) {
    y <- 13
  } else if (x>=0.5746 & x<0.6065) {
    y <- 14
  } else if (x>=0.6065 & x<0.6278) {
    y <- 15
  } else if (x>=0.6278 & x<0.6384) {
    y <- 16
  } else if (x>=0.6384 & x<0.6597) {
    y <- 17
  } else if (x>=0.6597 & x<0.6703) {
    y <- 18
  } else if (x>=0.6703 & x<0.6810) {
    y <- 19
  } else if (x>=0.6810 & x<0.7235) {
    y <- 20
  } else if (x>=0.7235 & x<0.7448) {
    y <- 21
  } else if (x>=0.7448 & x<0.7554) {
    y <- 22
  } else if (x>=0.7554 & x<0.7767) {
    y <- 24
  } else if (x>=0.7767 & x<0.7980) {
    y <- 26
  } else if (x>=0.7980 & x<0.8618) {
    y <- 27
  } else if (x>=0.8618 & x<0.8831) {
    y <- 28
  } else if (x>=0.8831 & x<0.8937) {
    y <- 29
  } else if (x>=0.8937 & x<0.9044) {
    y <- 30
  } else if (x>=0.9044 & x<0.9150) {
    y <- 31
  } else if (x>=0.9150 & x<0.9363) {
    y <- 32
  } else if (x>=0.9363 & x<0.9469) {
    y <- 35
  } else if (x>=0.9469 & x<0.9575) {
    y <- 38
  } else if (x>=0.9575 & x<0.9682) {
    y <- 39
  } else if (x>=0.9682 & x<0.9788) {
    y <- 42
  } else if (x>=0.9788 & x<0.9895) {
    y <- 46
  } else if (x>=0.9895 & x<=1) {
    y <- 47
  } else {
    y <- "ERROR"
  }
  
  return(y/100)
}

# build everything into a function

single_year_sim <- function(num_years,net_worth_start,contribution1,contribution1_years,contribution2,
                            contribution2_years,withdrawal_percent,max_withdrawal_amount) {
  
  years <- seq(1,num_years,1)
  contributions <- seq(1,num_years,1)
  withdrawals <- seq(0,num_years-1,1)
  rtn_input <- runif(num_years)
  sim_returns <- seq(1,num_years,1)
  net_worth <- seq(1,num_years,1)
  
  for (i in 1:num_years){
    if (i <= contribution1_years) {
      contributions[i] <- contribution1
    } else if (i > contribution1_years & i <= (contribution1_years + contribution2_years)){
      contributions[i] <- contribution2
    } else {
      contributions[i] <- 0
    }
  } #end for loop
  
  for (i in 1:num_years) {
    if (i == 1) {
      years[i] <- year(Sys.Date())
      sim_returns[i] <- rand_return(rtn_input[i])
      net_worth[i] <- net_worth_start
    } else {
      years[i] <- years[i-1]+1
      sim_returns[i] <- rand_return(rtn_input[i])
      if (contributions[i]> 0){
        withdrawals[i] <- 0
        net_worth[i] <- (net_worth[i-1] + contributions[i])*(1+sim_returns[i])
      } else {
        withdrawals[i] <- min(max(net_worth[i-1] * withdrawal_percent,0),max_withdrawal_amount)
        net_worth[i] <- (net_worth[i-1] - withdrawals[i])*(1+sim_returns[i])
      }
      
    }
  } #end for loop
  
  return(list(sim_returns,withdrawals,net_worth))
} # end function

sim_out <- single_year_sim(46,450000,28000,4,0,0,0.035,25000)
sim_out[1][[1]]
sim_out[2][[1]]
sim_out[3][[1]]

mean(sim_out[1][[1]])
