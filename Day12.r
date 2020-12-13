library(tidyverse)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day12.txt")

dir <- gsub("^(.).+$", "\\1", input)
amount <- gsub("^.(.+)$", "\\1", input) %>% as.numeric
# Part 1
cdir <- 90
x <- 0
y <- 0
for(i in 1:length(input)) {
  if(dir[i] == "N") {
    y <- y + amount[i]
  } else if(dir[i] == "E") {
    x <- x + amount[i]
  } else if(dir[i] == "S") {
    y <- y - amount[i]
  } else if(dir[i] == "W") {
    x <- x - amount[i]
  } else if(dir[i] == "L") {
    cdir <- (cdir - amount[i]) %% 360
  } else if(dir[i] == "R") {
    cdir <- (cdir + amount[i]) %% 360
  } else if(dir[i] == "F") {
    if(cdir == 0) {
      y <- y + amount[i]
    } else if(cdir == 90) {
      x <- x + amount[i]
    } else if(cdir == 180) {
      y <- y - amount[i]
    } else if(cdir == 270) {
      x <- x - amount[i]
    }
  }
}
abs(x) + abs(y)
# 582

get_angle <- function(x,y) {
  angle <- atan(y/x)
  if(x < 0) angle <- angle + pi
  return(angle)
}

# Part 2
x <- 10
y <- 1
sx <- 0
sy <- 0
for(i in 1:length(input)) {
  print(input[i])
  if(dir[i] == "N") {
    y <- y + amount[i]
  } else if(dir[i] == "E") {
    x <- x + amount[i]
  } else if(dir[i] == "S") {
    y <- y - amount[i]
  } else if(dir[i] == "W") {
    x <- x - amount[i]
  } else if(dir[i] == "L") {
    angle <- get_angle(x,y) + amount[i]*pi/180
    r <- sqrt(x^2 + y^2)
    x <- ( cos(angle) * r )
    y <- ( sin(angle) * r )
  } else if(dir[i] == "R") {
    angle <- get_angle(x,y) - amount[i]*pi/180
    r <- sqrt(x^2 + y^2)
    x <- ( cos(angle) * r )
    y <- ( sin(angle) * r )
  } else if(dir[i] == "F") {
    sy <- sy + y*amount[i]
    sx <- sx + x*amount[i]
  }
  cat(sx, sy, "\n")
  cat("::", x, y, "\n")
}
abs(sx) + abs(sy)
# 52069