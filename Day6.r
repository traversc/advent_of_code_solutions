library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

tablevec <- function(x) {
  out <- table(x)
  names <- names(out)
  out <- as.vector(out)
  names(out) <- names
  return(out)
}

input <- readChar("Data/Day6.txt", nchars = 1e7)
input <- strsplit(input, split = "\n\n") %>% unlist

# Part 1
map_int(input, function(x) {
  gsub("\n", "", x) %>% 
    strsplit("") %>%
    unlist %>%
    n_distinct
}) %>% sum
# 6726

# Part 2
map_int(input, function(x) {
  x <- strsplit(x, "\n") %>% unlist
  strsplit(x, "") %>% 
    unlist %>% 
    tablevec %>%
    {. == length(x)} %>% sum
}) %>% sum
# [1] 3316


