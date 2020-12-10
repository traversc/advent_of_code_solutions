library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day10.txt") %>% as.numeric %>% sort

# Part 1
(sum(diff(input) == 1)+1) * (sum(diff(input) == 3)+1)
# 2760

input <- readLines(textConnection("28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")) %>% as.numeric %>% sort

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
do.call(prod, results)