library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day11.txt") %>% strsplit("") %>%
  do.call(rbind, .)

# Part 1
check_position <- function(input, i, j) {
  if(i == 0 || j == 0 || i > nrow(input) || j > ncol(input)) return(0)
  input[i,j] == "#"
}
do_round <- function(input) {
  out <- input
  for(i in seq(1,nrow(input))) {
    for(j in seq(1,ncol(input))) {
      if(input[i,j] == ".") next;
      occ <- check_position(input,i,j+1) + 
      check_position(input,i,j-1) +
      check_position(input,i+1,j) + 
      check_position(input,i-1,j) +
      check_position(input,i+1,j+1) +
      check_position(input,i+1,j-1) +
      check_position(input,i-1,j+1) +
      check_position(input,i-1,j-1)
      if(occ >= 4) {
        out[i,j] = "L"
      } else if(occ == 0) {
        out[i,j] = "#"
      }
    }
  }
  out
}

previous_input <- input
input <- do_round(input)
while(any(input != previous_input)) {
  print(sum(input == "#"))
  previous_input <- input
  input <- do_round(previous_input)
}
# [1] 2476

# Part 2
adjacency <- function(input, i, j, di, dj) {
  i <- i + di
  j <- j + dj
  while(i != 0 && j != 0 && i <= nrow(input) && j <= ncol(input)) {
    if(input[i,j] == "L") {
      return(c(i,j))
    }
    i <- i + di
    j <- j + dj
  }
  c(NA, NA)
}

x <- numeric(sum(input == "L"))
y <- numeric(sum(input == "L"))
adj <- matrix(nrow=sum(input == "L"), ncol = 16)
count <- 0
for(i in seq(1,nrow(input))) {
  for(j in seq(1,ncol(input))) {
    if(input[i,j] == "L") {
      count <- count + 1
      x[count] <- i
      y[count] <- j
      adj[count,1:2] <- adjacency(input,i,j,0,+1)
      adj[count,3:4] <- adjacency(input,i,j,0,-1)
      adj[count,5:6] <- adjacency(input,i,j,+1,0)
      adj[count,7:8] <- adjacency(input,i,j,-1,0)
      adj[count,9:10] <- adjacency(input,i,j,+1,+1)
      adj[count,11:12] <- adjacency(input,i,j,+1,-1)
      adj[count,13:14] <- adjacency(input,i,j,-1,+1)
      adj[count,15:16] <- adjacency(input,i,j,-1,-1)
    }
  }
}

check_position <- function(input,ij) {
  if(is.na(ij[1]) || is.na(ij[2])) {
    0
  } else {
    ifelse(input[ij[1],ij[2]] == "#", 1, 0)
  }
}
do_round <- function(input) {
  out <- input
  for(q in 1:nrow(adj)) {
    occ <- 
      check_position(input,adj[q,1:2]) + 
      check_position(input,adj[q,3:4]) + 
      check_position(input,adj[q,5:6]) + 
      check_position(input,adj[q,7:8]) + 
      check_position(input,adj[q,9:10]) + 
      check_position(input,adj[q,11:12]) + 
      check_position(input,adj[q,13:14]) + 
      check_position(input,adj[q,15:16])
    if(occ >= 5) {
      out[x[q],y[q]] <- "L"
    } else if(occ == 0) {
      out[x[q],y[q]] <- "#"
    }
  }
  out
}

previous_input <- input
input <- do_round(input)
while(any(input != previous_input)) {
  print(sum(input == "#"))
  previous_input <- input
  input <- do_round(previous_input)
}
# 2257