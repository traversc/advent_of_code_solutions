library(tidyverse)
library(data.table)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day19.txt")
w <- which(input == "")
data <- input[(w+1):length(input)]

rules <- input[1:(w-1)]

# rules <- "0: 4 1 5
# 1: 2 3 | 3 2
# 2: 4 4 | 5 5
# 3: 4 5 | 5 4
# 4: a
# 5: b"
# rules <- strsplit(rules, "\n") %>% unlist

rules_index <- gsub("^(.+): .+$", "\\1", rules) %>% as.numeric
rules <- gsub("^(.+): (.+)$", "\\2", rules)
rules <- gsub('\\"', "", rules)

rule_list <- character(length(rules))
rule_list[rules_index + 1] <- rules

parse_rule <- function(r) {
  if(r == "a") return("a")
  if(r == "b") return("b")
  if( ! suppressWarnings(is.na(as.numeric(r))) ) {
    return(parse_rule(rule_list[as.integer(r)+1]))
  }
  rl <- strsplit(r, split = " \\| ") %>% unlist
  results <- list()
  for(i in 1:length(rl)) {
    rll <- rl[i]
    rlll <- strsplit(rll, " ") %>% unlist
    z <- lapply(rlll, parse_rule)
    z <- do.call(expand.grid, z)
    results[[i]] <- do.call(paste0, z)
  }
  results %>% unlist
}

# Part 1
exp_rule <- parse_rule(rule_list[1])
sum(data %in% exp_rule)
# 226


# Part 2

mgrepl <- function(patterns, x, ...) {
  Reduce("|", lapply(patterns, grepl, x, ...))
}

rule_list[8+1] <- "42 | 42 8"
rule_list[11+1] <- "42 31 | 42 11 31"

r42 <- parse_rule(42)
r31 <- parse_rule(31)

nc42 <- nchar(r42) %>% unique
nc31 <- nchar(r31) %>% unique


grid <- expand.grid(n8 = 1:12, n11 = 1:6) %>%
  mutate(nc = n8 * 8 + n11 * 16) %>%
  filter(nc <= 96)

check_dg <- function(d, n8, n11) {
  for(k in 1:n8) {
    m <- mgrepl(paste0("^", r42), d)
    if(!m) return(F)
    d <- substr(d, nc42+1, nchar(d))
  }
  for(k in 1:n11) {
    m <- mgrepl(paste0("^", r42), d)
    if(!m) return(F)
    d <- substr(d, nc42+1, nchar(d))
  }
  for(k in 1:n11) {
    m <- mgrepl(paste0("^", r31), d)
    if(!m) return(F)
    d <- substr(d, nc31+1, nchar(d))
  }
  return(T)
}

res <- logical(length(data))
for(i in 1:length(data)) {
  print(i)
  g <- filter(grid, nc == nchar(data[i]))
  res[i] <- sapply(1:nrow(g), function(j) {
    check_dg(d=data[i], n8=g$n8[j], n11=g$n11[j])
  }) %>% any
}
sum(res)
# 355