library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/day1.txt") %>% as.numeric

x <- data.frame(x = input)
y <- data.frame(y = input)

# Part 1
crossing(x,y) %>% 
  filter(x > y) %>%
  mutate(sum = x + y) %>%
  filter(sum == 2020) %>% 
  mutate(prod = x * y) %>%
  pull(prod) %>%
  print
# [1] 877971

# Part 2
z <- data.frame(z = input)
crossing(x,y) %>% 
  filter(x > y) %>%
  mutate(sum = x + y) %>%
  filter(sum < 2020) %>% 
  crossing(z) %>%
  filter(y > z) %>%
  mutate(sum = sum + z) %>%
  filter(sum == 2020) %>%
  mutate(prod = x * y * z) %>%
  pull(prod) %>%
  print
# [1] 203481432