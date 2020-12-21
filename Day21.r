library(tidyverse)
library(data.table)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day21.txt")
re <- "^(.+)\\(contains (.+)\\)$"
allergens <- gsub(re, "\\2", input) %>% gsub(" ", "", .) %>%
  strsplit(",")

ingredients <- gsub(re, "\\1", input) %>%
  trimws %>%
  strsplit(" ")

uallergens <- unlist(allergens) %>% unique

res <- list()
for(i in 1:length(uallergens)) {
  ua <- uallergens[i]
  ua_ingredients <- list()
  for(j in 1:length(ingredients)) {
    if(!ua %in% allergens[[j]]) next;
    ua_ingredients[[j]] <- ingredients[[j]]
  }
  ua_ingredients <- ua_ingredients[!sapply(ua_ingredients, is.null)]
  res[[i]] <- Reduce(intersect, ua_ingredients)
}
names(res) <- uallergens

# Part 1
itab <- unlist(ingredients) %>% table
itab[!names(itab) %in% unlist(res)] %>% sum
# 1958

# Part 2 -- simple to figure manually
print(res)
# $shellfish
# [1] X"xxscc"   X"gbcjqbm" X"gzxnc"  
# $peanuts
# [1] X"vvqj"    X"trnnvn"  X"gbcjqbm"
# $nuts
# [1] X"nckqzsg" X"xxscc"   X"gzxnc"  
# $wheat
# [1] X"nckqzsg"
# $fish
# [1] X"nckqzsg" X"xxscc"   X"mjmqst" 
# $eggs
# [1] X"xxscc"   X"nckqzsg"
# $soy
# [1] X"nckqzsg" X"dllbjr"  X"gzxnc"  
# $sesame
# [1] X"trnnvn"  X"nckqzsg" X"gzxnc"   X"gbcjqbm"

res <- fread(text = "
wheat, nckqzsg
eggs, xxscc
fish, mjmqst
nuts, gzxnc
soy, dllbjr
shellfish, gbcjqbm
sesame, trnnvn
peanuts, vvqj", header=F)

res %>% arrange(V1) %>% pull(V2) %>% paste0(collapse=",")
#[1] "xxscc,mjmqst,gzxnc,vvqj,trnnvn,gbcjqbm,dllbjr,nckqzsg"

