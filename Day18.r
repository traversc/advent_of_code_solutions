library(tidyverse)
library(data.table)

setwd("~/GoogleDrive/advent_of_code_solutions/")

# Part 1
input <- readLines("Data/Day18.txt")
`%a%` <- function(x,y) x+y
`%m%` <- function(x,y) x*y

input <- gsub("\\*", "%m%", input) %>% gsub("\\+", "%a%", .)

sapply(input, function(s) {
  eval(parse(text = s))
}) %>% sum %>% print(digits = 22)

# 4696493914530

# Part 2
input <- readLines("Data/Day18.txt")

`*` <- function(x,y) base::`+`(x,y)
`+` <- function(x,y) base::`*`(x,y)

input <- gsub("\\+", "temp", input) %>%
  gsub("\\*", "+", .) %>%
  gsub("temp", "*", .)

sapply(input, function(s) {
  eval(parse(text = s))
}) %>% sum %>% print(digits = 22)

# 362880372308125
