library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")
input <- readChar("Data/Day4.txt", nchar = 1e7)

required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

input <- strsplit(input, split = "\n\n") %>% unlist
input <- map(input, function(x) {
  x <- strsplit(x, "[\n| ]") %>% unlist
  x <- strsplit(x, ":") %>% do.call(rbind, .)
  out <- x[,2]
  names(out) <- x[,1]
  out
})

# Part 1
map_lgl(input, ~ all(required_fields %in% names(.x)) ) %>% sum
# 242

# Part 2
map_lgl(input, function(x) {
  if( ! all(required_fields %in% names(x)) ) return(F)
  if(!x["byr"] %in% 1920:2002) return(F)
  if(!x["iyr"] %in% 2010:2020) return(F)
  if(!x["eyr"] %in% 2020:2030) return(F)
  hgt_val <- gsub("^([0-9]+)(in|cm)$", "\\1", x["hgt"])
  hgt_units <- gsub("^([0-9]+)(in|cm)$", "\\2", x["hgt"])
  if(hgt_units == "cm") {
    if(!hgt_val %in% 150:193) return(F)
  } else {
    if(!hgt_val %in% 59:76) return(F)
  }
  if(!grepl("^#[0-9|a-f]{6}$", x["hcl"])) return(F)
  if(!x["ecl"] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) return(F)
  if(nchar(x["pid"]) != 9) return(F)
  pid_digits <- strsplit(x["pid"], "") %>% unlist %>% unique
  if(!all(pid_digits %in% 0:9)) return(F)
  T
}) %>% sum
# 186

