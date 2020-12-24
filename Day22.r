library(dplyr)
library(R6)
library(Rcpp)

setwd("~/GoogleDrive/advent_of_code_solutions/")
sourceCpp("Day22.cpp")

Queue <- R6Class("Queue",
  public = list(
    v = NULL,
    initialize = function(v) {
      self$v <- v
    },
    pop = function() {
      p <- self$v[1]
      self$v <- self$v[-1]
      return(p)
    },
    push = function(vnew) {
      self$v <- c(self$v, vnew)
    },
    length = function() {
      return(length(self$v))
    }
  )
)

input <- readLines("Data/Day22.txt")
w <- which(input == "")
p1 <- input[2:(w-1)] %>% as.numeric
p2 <- input[(w+2):length(input)] %>% as.numeric

v1 <- Queue$new(p1)
v2 <- Queue$new(p2)

# Part 1
while(v1$length() > 0 && v2$length() > 0) {
  card1 <- v1$pop()
  card2 <- v2$pop()
  if(card1 > card2) {
    v1$push(c(card1, card2))
  } else {
    v2$push(c(card2, card1))
  }
}

if(v1$length() > 0) {
  win <- v1$v
} else {
  win <- v2$v
}

sum(win * seq(length(win), 1))
# 33925

# Part 2
library(Rcpp)
library(dplyr)
sourceCpp("Day22.cpp")
input <- readLines("Data/Day22.txt")
w <- which(input == "")
p1 <- input[2:(w-1)] %>% as.integer
p2 <- input[(w+2):length(input)] %>% as.integer
win <- run_game(p1, p2)

win <- c(25,2,17,12,24,9,27,21,50,37,47,28,38,20,26,13,36,30,45,16,42,7,33,14,49,48,34,19,46,32,1,6,44,23,22,5,40,29,43,18,35,3,41,11,31,8,39,10,15,4)
sum(win * seq(length(win), 1))
# 33441