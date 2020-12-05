library(tidyverse)
library(data.table)

setwd("~/GoogleDrive/advent_of_code_solutions/")
input <- readLines("Data/Day3.txt")
input <- strsplit(input, "") %>% do.call(rbind, .)

sol <- function(r = 3, d = 1) {
  x <- 1
  y <- 1
  trees <- 0
  while(y <= nrow(input)) {
    trees <- trees + (input[y,x] == "#")
    x <- x + r
    if(x > ncol(input)) {
      x <- x - ncol(input)
    }
    y <- y + d
  }
  trees
}

# Part 1
sol()
# [1] 167

# Part 2
sol(1) * sol(3) * sol(5) * sol(7) * sol(1,2)
# [1] 736527114