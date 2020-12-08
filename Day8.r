library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day8.txt")
re <- "^(.+) (.+)$"
input <- data.frame(op = gsub(re, "\\1", input), arg = gsub(re, "\\2", input) %>% as.numeric)

# Part 1
visited <- rep(FALSE, length.out = nrow(input))
acc <- 0
line <- 1
while(TRUE) {
  if(visited[line]) break;
  visited[line] <- TRUE
  if(input$op[line] == "jmp") {
    line <- line + input$arg[line]
  } else if(input$op[line] == "acc") {
    acc <- acc + input$arg[line]
    line <- line + 1
  } else { # nop
    line <- line + 1
  }
}
print(acc)
# 1915

# Part 2
test_terminate <- function(input, modline) {
  input$op[modline] <- ifelse(input$op[modline] == "nop", "jmp", "nop")
  visited <- rep(FALSE, length.out = nrow(input))
  acc <- 0
  line <- 1
  while(line <= nrow(input)) {
    if(visited[line]) break;
    visited[line] <- TRUE
    if(input$op[line] == "jmp") {
      line <- line + input$arg[line]
    } else if(input$op[line] == "acc") {
      acc <- acc + input$arg[line]
      line <- line + 1
    } else { # nop
      line <- line + 1
    }
  }
  if(line <= nrow(input)) {
    -1
  } else {
    acc
  }
}

for(i in 1:nrow(input)) {
  acc <- test_terminate(input, i)
  if(acc != -1) break
}
print(acc)
# 944
