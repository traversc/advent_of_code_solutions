library(tidyverse)
library(data.table)
setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day14.txt")
# "mask = 000000000000000000000000000000X1001X
# mem[42] = 100
# mask = 00000000000000000000000000000000X0XX
# mem[26] = 1" %>% strsplit("\n") %>% unlist -> input

mask <- grep("mask", input, value=T) %>% 
  gsub("mask = ", "", .) %>% 
  strsplit("") %>%
  lapply(function(s) {
    rbind(
      data.frame(replace = 1, position = which(s == "1")),
      data.frame(replace = 0, position = which(s == "0")) )
  })

n <- 0
memdf <- list()
for(i in 1:length(input)) {
  if(grepl("mask", input[i])) {
    n <- n+1
  } else {
    mem <- grep("mem", input[i], value=T) %>%
      gsub("^mem\\[(.+)\\] = (.+)$", "\\1", .) %>%
      as.numeric
    val <- grep("mem", input[i], value=T) %>%
      gsub("^mem\\[(.+)\\] = (.+)$", "\\2", .) %>%
      as.numeric
    memdf[[i]] <- data.frame(mask = n, mem = mem, val = val)
  }
}
memdf <- rbindlist(memdf)
memdf$bin <- lapply(memdf$val, function(m) {
  x <- rep(0, 36)
  x[1:32] <- intToBits(m) %>% as.integer
  rev(x)
})

# Part 1
res <- list()
for(i in 1:nrow(memdf)) {
  x <- memdf$bin[[i]]
  m <- mask[[memdf$mask[i]]]
  x[m$position] <- m$replace
  x <- sum(2^(35:0) * x)
  res[[as.character(memdf$mem[i])]] <- x
}
sum(unlist(res)) %>% print(digits = 22)
# 6386593869035

# Part 2
mask <- grep("mask", input, value=T) %>% 
  gsub("mask = ", "", .) %>% 
  strsplit("") %>%
  lapply(function(s) {
      data.frame(replace = s, position = 1:length(s))
  })

n <- 0
memdf <- list()
for(i in 1:length(input)) {
  if(grepl("mask", input[i])) {
    n <- n+1
  } else {
    mem <- grep("mem", input[i], value=T) %>%
      gsub("^mem\\[(.+)\\] = (.+)$", "\\1", .) %>%
      as.numeric
    val <- grep("mem", input[i], value=T) %>%
      gsub("^mem\\[(.+)\\] = (.+)$", "\\2", .) %>%
      as.numeric
    memdf[[i]] <- data.frame(mask = n, mem = mem, val = val)
  }
}
memdf <- rbindlist(memdf)
memdf$bin <- lapply(memdf$mem, function(m) {
  x <- rep(0, 36)
  x[1:32] <- intToBits(m) %>% as.integer
  rev(x)
})

res <- list()
for(i in 1:nrow(memdf)) {
  x <- memdf$bin[[i]]
  m <- mask[[memdf$mask[i]]]
  mx <- filter(m, replace == "X")
  m1 <- filter(m, replace == "1")
  x[m1$position] <- 1
  args <- lapply(1:nrow(mx), function(x) c(0,1))
  names(args) <- paste0("X", mx$position)
  grid <- do.call(expand.grid, args)
  
  for(j in 1:nrow(grid)) {
    x2 <- x
    x2[mx$position] <- grid[j,] %>% unlist
    x2 <- as.character(sum(2^(35:0) * x2))
    res[[x2]] <- memdf$val[i]
  }
}
sum(unlist(res)) %>% print(digits = 22)
# 4288986482164