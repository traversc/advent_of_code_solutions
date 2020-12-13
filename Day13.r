library(tidyverse)
setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day13.txt")

# Part 1
start <- as.numeric(input[1])
buses <- strsplit(input[2], ",") %>% 
  unlist %>%
  unique %>%
  setdiff("x") %>%
  as.numeric

wb <- sapply(buses, function(b) {
  ceiling(start / b) * b
})
buses[which.min(wb)] * (min(wb) - start)
# 3035

# part 2
buses <- strsplit(input[2], ",") %>% 
  unlist %>%
  as.numeric
buses <- data.frame(position = which(!is.na(buses))-1, 
                    bus = na.omit(buses))
buses$remainder <- sapply(1:nrow(buses), function(i) {
  for(j in 0:100) {
    r <- (buses$bus[i] * j) - buses$position[i]
    if(r >= 0) return(r)
  }
})

# Chinese remainder theorem
# https://www.youtube.com/watch?v=ru7mWZJlRQg
N <- prod(buses$bus)
z <- N / buses$bus
x <- sapply(1:nrow(buses), function(i) {
  for(j in 0:10000) {
    test <- z[i]*j
    if((test %% buses$bus[i]) == buses$remainder[i]) return(test)
  }
  stop(":(")
})
ans <- sum(x) %% N

# check
for(i in 1:nrow(buses)) {
  print( (ans + buses$position[i]) %% buses$bus[i] )
}
print(ans, digits = 22)
# 725169163285238
