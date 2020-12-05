library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")
input <- readLines("Data/Day5.txt")
input_fb <- substr(input, 1, 7) %>% 
  gsub("B", "1", .) %>%
  gsub("F", "0", .)
input_fb <- strtoi(input_fb, base = 2)

input_rl <- substr(input, 8, 10) %>% 
  gsub("R", "1", .) %>%
  gsub("L", "0", .)
input_rl <- strtoi(input_rl, base = 2)

seat_id <- input_fb * 8 + input_rl

# Part 1
max(seat_id)
# 835

# Part 2
setdiff(min(seat_id):max(seat_id), seat_id)
# [1] 649