# load libraries
library(tidyverse)

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
}

bin_counts$left

for (i in 1:dim(bin_counts)[1]){
  if (i==dim(bin_counts)[1]) {
    bin_counts$right[i] <- 1.000
  } else {
    bin_counts$right[i] <- round(bin_counts[i,"cum_percent"][[1]],4)
  }
}

bin_counts$right
