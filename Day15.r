library(tidyverse)
library(data.table)
library(Rcpp)
setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- c(0,13,1,8,6,15)
# input <- c(0,3,6)

# Part 1
n <- 2020 - length(input)
seq <- rep(NA,2020)
seq[1:length(input)] <- input
age_hash <- list()
for(i in 1:(length(input)-1)) {
  age_hash[[as.character(seq[i])]] <- i
}
for(i in (length(input)+1:n)) {
  h <- age_hash[[as.character(seq[i-1])]]
  if(is.null(h)) {
    seq[i] <- 0
  } else {
    seq[i] <- (i-1) - h
  }
  age_hash[[as.character(seq[i-1])]] <- i-1
}
tail(seq)
# 1618

# Part 2
# Same algorithm but R is way too slow
# needs to be written in C++

sourceCpp(code = "#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int> day15_cpp(std::vector<int> input, size_t len) {
  std::vector<int> seq(len);
  for(size_t i=0; i<input.size(); ++i) {
    seq[i] = input[i];
  }
  std::unordered_map<int, int> age_hash;
  for(size_t i=0; i<input.size() - 1; ++i) {
    age_hash.emplace(input[i], i);
  }
  for(size_t i=input.size(); i<len; ++i) {
    if(age_hash.find(seq[i-1]) == age_hash.end()) {
      seq[i] = 0;
      age_hash.emplace(seq[i-1], i-1);
    } else {
      seq[i] = (i-1) - age_hash.at(seq[i-1]);
      age_hash.at(seq[i-1]) = i-1;
    }
  }
  return seq;
}
")

input <- c(0,13,1,8,6,15)
res <- day15_cpp(input, 30000000)
tail(res)
# 548531