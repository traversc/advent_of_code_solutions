library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

tablevec <- function(x) {
  out <- table(x)
  names <- names(out)
  out <- as.vector(out)
  names(out) <- names
  return(out)
}

input <- readLines("Data/day2.txt")
re <- "^([0-9]+)-([0-9]+) (.): (.+)$"
input <- data.frame(V1=gsub(re, "\\1", input) %>% as.numeric,
                    V2=gsub(re, "\\2", input) %>% as.numeric,
                    V3=gsub(re, "\\3", input),
                    V4=gsub(re, "\\4", input))

# Part 1
map_lgl(1:nrow(input), function(i) {
  x <- strsplit(input$V4[i], "") %>% 
    unlist %>% 
    factor(levels = letters) %>%
    tablevec
  x[input$V3[i]] %in% input$V1[i]:input$V2[i]
}) %>% sum
# [1] 625

# Part 2
map_lgl(1:nrow(input), function(i) {
  x <- strsplit(input$V4[i], "") %>% unlist
  p1 <- x[input$V1[i]] == input$V3[i]
  p2 <- x[input$V2[i]] == input$V3[i]
  p1 + p2 == 1
}) %>% sum
# [1] 391

