# day 11
library(tidyverse)
input<-read_fwf("2021/day11input.txt", fwf_widths(rep(1,10)))

# part1
get_window <- function(m,r,c) {
  window <- m[c(r-1,r,r+1), c(c-1,c,c+1)]
  return(window)
}

result <- c()
steps <- list()
step <- input
step[,11] <- NA
for(i in 1:100){
  step <- step+1
  expired_flashes <- 0
  active_flashes <- length(which(step>9))
  while(active_flashes > 0){
    step[step>9] <- 99
    for(r in 1:10){
      for(c in 1:10){
        if(any(get_window(step, r ,c)==99, na.rm = TRUE)
           & step[r,c] != 99 
           & step[r,c] != 0){
          n_flashes <- which(get_window(step, r ,c)==99) %>% length()
          step[r,c] <- step[r,c] + n_flashes}
        
      }
    }
    expired_flashes <- expired_flashes + length(which(step == 99))
    step[step == 99] <- 0
    active_flashes <- length(which(step>9))
  }

  result[i] <- expired_flashes
  print(i)
}
sum(result)

result <- c()
step <- input
step[,11] <- NA
counter <- 1
while(expired_flashes < 100){
  step <- step+1
  expired_flashes <- 0
  active_flashes <- length(which(step>9))
  while(active_flashes > 0){
    step[step>9] <- 99
    for(r in 1:10){
      for(c in 1:10){
        if(any(get_window(step, r ,c)==99, na.rm = TRUE)
           & step[r,c] != 99 
           & step[r,c] != 0){
          n_flashes <- which(get_window(step, r ,c)==99) %>% length()
          step[r,c] <- step[r,c] + n_flashes}
        
      }
    }
    expired_flashes <- expired_flashes + length(which(step == 99))
    step[step == 99] <- 0
    active_flashes <- length(which(step>9))
  }
  
  result[i] <- expired_flashes
  print(paste(counter, expired_flashes))
  counter <- counter +1
}
sum(result)