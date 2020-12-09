library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day9.txt") %>% as.numeric

# Part 1
for(i in 26:length(input)) {
  x <- input[(i-25):i]
  x <- combn(x,2) %>% colSums
  if(all(x != input[i])) break
}
print(input[i])
# 400480901

# Part 2
val <- input[i]
test <- function() {
  for(i in 1:(length(input)-1)) {
    for(j in i:length(input)) {
      if(val == sum(input[i:j])) {
        print(sum(input[i:j]))
        return(c(i,j))
      }
    }
  }
}
results <- test()
range(input[results[1]:results[2]]) %>% sum
# 67587168