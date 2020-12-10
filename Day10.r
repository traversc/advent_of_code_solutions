library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day10.txt") %>% as.numeric %>% sort

# Part 1
(sum(diff(input) == 1)+1) * (sum(diff(input) == 3)+1)
# 2760

# Part 2
connect <- function(input, pos) {
  total <- 0
  if(pos == length(input)) return(1)
  for(i in seq(pos+1, length(input))) {
    if(input[i] > input[pos] + 3) break
    total <- total + connect(input, i)
  }
  total
}

input <- c(0, input, max(input)+3)
split <- c(1, which(diff(input) == 3), length(input))
results <- list()
for(i in 2:length(split)) {
  new_input <- input[split[i-1]:split[i]]
  results[[i-1]] <- connect(new_input, 1)
}
print(do.call(prod, results), digits = 22)
# 13816758796288