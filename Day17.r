library(tidyverse)
library(data.table)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day17.txt") %>%
  strsplit("") %>% 
  do.call(rbind, .)


# Part 1
get_neighbors <- function(s) {
  s <- strsplit(s, ":") %>% unlist %>% as.numeric
  x <- s[1]
  y <- s[2]
  z <- s[3]
  s <- expand.grid(x=c(x, x+1, x-1),
                   y=c(y, y+1, y-1),
                   z=c(z, z+1, z-1))[-1,]
  paste(s$x, s$y, s$z, sep = ":")
}

active <- c()
for(i in 1:nrow(input)) {
  for(j in 1:ncol(input)) {
    if(input[i,j] == "#") {
      active <- c(active, paste(i-1, j-1, 0, sep = ":"))
    }
  }
}

for(. in 1:6) {
  nbs <- vector(mode = "list", length = length(active))
  for(i in 1:length(active)) {
    nbs[[i]] <- get_neighbors(active[i])
  }
  nbs <- unlist(nbs) %>% unique
  nb_active <- which(nbs %in% active)
  nb_inactive <- setdiff(1:length(nbs), nb_active)
  update <- rep(F, length(nbs))
  for(i in nb_active) {
    x <- sum(get_neighbors(nbs[i]) %in% active)
    if(x %in% c(2,3)) {
      update[i] <- T
    }
  }
  for(i in nb_inactive) {
    x <- sum(get_neighbors(nbs[i]) %in% active)
    if(x == 3) {
      update[i] <- T
    }
  }
  active <- nbs[update]
  print(length(active))
}
# 368


# Part 2
get_neighbors <- function(s) {
  s <- strsplit(s, ":") %>% unlist %>% as.numeric
  x <- s[1]
  y <- s[2]
  z <- s[3]
  w <- s[4]
  s <- expand.grid(x=c(x, x+1, x-1),
                   y=c(y, y+1, y-1),
                   z=c(z, z+1, z-1),
                   w=c(w, w+1, w-1))[-1,]
  paste(s$x, s$y, s$z, s$w, sep = ":")
}

active <- c()
for(i in 1:nrow(input)) {
  for(j in 1:ncol(input)) {
    if(input[i,j] == "#") {
      active <- c(active, paste(i-1, j-1, 0, 0, sep = ":"))
    }
  }
}

for(. in 1:6) {
  nbs <- vector(mode = "list", length = length(active))
  for(i in 1:length(active)) {
    nbs[[i]] <- get_neighbors(active[i])
  }
  nbs <- unlist(nbs) %>% unique
  nb_active <- which(nbs %in% active)
  nb_inactive <- setdiff(1:length(nbs), nb_active)
  update <- rep(F, length(nbs))
  for(i in nb_active) {
    x <- sum(get_neighbors(nbs[i]) %in% active)
    if(x %in% c(2,3)) {
      update[i] <- T
    }
  }
  for(i in nb_inactive) {
    x <- sum(get_neighbors(nbs[i]) %in% active)
    if(x == 3) {
      update[i] <- T
    }
  }
  active <- nbs[update]
  print(length(active))
}
# 2696

