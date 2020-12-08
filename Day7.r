library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day7.txt")

re <- "^(.+) bags? contains?(.+)$"
container <- gsub(re, "\\1", input)
contents <- gsub(re, "\\2", input) %>% strsplit(",|\\.")

re2 <- "^([0-9]+) (.+) bags?$"
contents <- lapply(contents, function(x) {
  x <- trimws(x)
  if(x[1] %like% "no other bags") {
    data.frame(color = character(), n = numeric())
  } else {
    data.frame(color = gsub(re2, "\\2", x), n = gsub(re2, "\\1", x) %>% as.numeric)
  }
})
names(contents) <- container

recurse_bag <- function(bag) {
  current_contents <- contents[[bag]]
  all_bags <- contents[[bag]]
  if(nrow(all_bags) == 0) return(all_bags)
  for(i in 1:nrow(current_contents)) {
    temp <- recurse_bag(current_contents$color[i])
    temp$n <- temp$n * current_contents$n[i]
    all_bags <- rbind(all_bags, temp)
  }
  all_bags
}

# Part 1
total <- 0
for(i in 1:length(contents)) {
  print(i)
  current_contents <- contents[[i]]
  z <- recurse_bag(names(contents)[i])
  if("shiny gold" %in% z$color) total <- total + 1
}
# 126

# Part 2
recurse_bag("shiny gold")$n %>% sum
# 220149
