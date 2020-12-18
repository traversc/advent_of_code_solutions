library(tidyverse)
library(data.table)
library(IRanges)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day16.txt")

w1 <- which(input == "your ticket:")
w2 <- which(input == "nearby tickets:")

rules <- input[1:(w1-2)]
re <- "^(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$"
rules <- data.frame(field = gsub(re, "\\1", rules),
                    min1 = gsub(re, "\\2", rules) %>% as.numeric,
                    max1 = gsub(re, "\\3", rules) %>% as.numeric,
                    min2 = gsub(re, "\\4", rules) %>% as.numeric,
                    max2 = gsub(re, "\\5", rules) %>% as.numeric)

rules_flat <- IRanges(c(rules$min1, rules$min2), c(rules$max1, rules$max2)) %>% 
  reduce %>%
  as.data.frame

myticket <- input[(w1+1)] %>% strsplit(",") %>% unlist %>% as.numeric
nearby <- input[(w2+1):length(input)] %>%
  strsplit(",") %>% lapply(as.numeric)

# Part 1

total <- 0
for(i in 1:length(nearby)) {
  nb <- nearby[[i]]
  total <- total + sum(nb[nb < rules_flat$start | nb > rules_flat$end])
}
print(total)
# [1] 27802

# Part 2

nearby <- nearby[sapply(nearby, function(nb) {
  all(nb >= rules_flat$start & nb <= rules_flat$end)
})]

nearby <- do.call(rbind, nearby)

consistent <- lapply(1:nrow(rules), function(i) {
  sapply(1:ncol(nearby), function(j) {
    nb <- nearby[,j]
    z <- (nb >= rules$min1[i] & nb <= rules$max1[i]) | (nb >= rules$min2[i] & nb <= rules$max2[i])
    all(z)
  }) %>% which
})

fields <- numeric(20)
len <- sapply(consistent, length)
for(i in 1:nrow(rules)) {
  w <- which(len == i)
  fields[w] <- setdiff(consistent[[w]], fields)
}

myf <- fields[which(rules$field %like% "departure")]
myticket[myf] %>% prod
# 279139880759


