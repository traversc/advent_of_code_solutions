library(tidyverse)
library(data.table)
library(parallel)

setwd("~/GoogleDrive/advent_of_code_solutions/")

rotate <- function(mat) t(mat[nrow(mat):1,])
flip <- function(mat) mat[,ncol(mat):1]

input <- readLines("Data/Day20.txt")
w <- which(input %like% "Tile")
labels <- input[w] %>% gsub("^Tile ([0-9]+):$", "\\1", .) %>% as.numeric

tiles <- list()
for(i in 1:length(w)) {
  if(i == length(w)) {
    tl <- input[seq(w[i]+1, length(input)-1)]
  } else {
    tl <- input[seq(w[i]+1, w[i+1]-2)]
  }
  tl <- strsplit(tl, "")
  tl <- do.call(rbind, tl)
  tlf <- flip(tl)
  rot <- list(tl, tlf)
  for(j in 1:3) {
    tl <- rotate(tl)
    rot[[length(rot) + 1]] <- tl
  }
  for(j in 1:3) {
    tlf <- rotate(tlf)
    rot[[length(rot) + 1]] <- tlf
  }
  tiles[[i]] <- rot
}

tiles <- unlist(tiles, recursive=F)
names(tiles) <- paste0(rep(labels, each=8), "_", 1:8)

match_tiles <- function(x, y) {
  ret <- ""
  if( all(x[1,] == y[10,]) ) { if(ret != "") stop(":("); ret <- "N" }
  if( all(x[,10] == y[,1]) ) { if(ret != "") stop(":("); ret <- "E" }
  if( all(x[10,] == y[1,]) ) { if(ret != "") stop(":("); ret <- "S" }
  if( all(x[,1] == y[,10]) ) { if(ret != "") stop(":("); ret <- "W" }
  ret
}

matches <- list()
for(i in 1:length(tiles)) {
  print(i)
  matches[[i]] <- sapply(1:length(tiles), function(j) {
    if( substr(names(tiles)[i], 1, 4) == substr(names(tiles)[j], 1, 4) ) return("")
    match_tiles(x=tiles[[i]], y=tiles[[j]])
  })
}
names(matches) <- names(tiles)

sm <- sapply(matches, function(m) {
  sum(m != "")
})

# Part 1
names(tiles)[sm == 2] %>% gsub("_.", "", .) %>% unique %>% 
  as.numeric %>% prod %>% print(digits = 22)

corners <- names(tiles)[which(sm == 2)] %>% gsub("_.", "", .) %>% unique
edges <- names(tiles)[which(sm== 3)] %>% gsub("_.", "", .) %>% unique
middle <- names(tiles)[which(sm == 4)] %>% gsub("_.", "", .) %>% unique

idmat <- matrix(nrow=12, ncol=12)
topleft <- sapply(names(tiles)[which(sm == 2)], function(cn) {
  "E" %in% matches[[cn]] && "S" %in% matches[[cn]]
}) %>% which %>% names %>% grep("_1", ., value=T)
idmat[1,1] <- topleft

# north face
for(i in 2:12) {
  m <- matches[[idmat[1,i-1]]]
  idmat[1,i] <- names(tiles)[which(m == "E")]
}
# east face
for(i in 2:12) {
  m <- matches[[idmat[i-1,12]]]
  idmat[i,12] <- names(tiles)[which(m == "S")]
}
# south face
for(i in 11:1) {
  m <- matches[[idmat[12,i+1]]]
  idmat[12,i] <- names(tiles)[which(m == "W")]
}
# east face
for(i in 11:1) {
  m <- matches[[idmat[i+1,1]]]
  idmat[i,1] <- names(tiles)[which(m == "N")]
}
# middle
for(i in 2:11) {
  for(j in 2:11) {
    m <- matches[[idmat[i,j-1]]]
    idmat[i,j] <- names(tiles)[which(m == "E")]
    # check
    m <- matches[[idmat[i-1,j]]]
    stopifnot(idmat[i,j] == names(tiles)[which(m == "S")])
  }
}

picture <- matrix(nrow = 8*12, ncol = 8*12)
for(i in 1:12) {
  for(j in 1:12) {
    tl <- tiles[[idmat[i,j]]]
    tl <- tl[-c(1,10),][,-c(1,10)]
    picture[seq((i-1)*8+1,i*8),seq((j-1)*8+1,j*8)] <- tl
  }
}

sea_monster <- "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "
sea_monster <- sea_monster %>% strsplit("\n") %>%
  unlist %>%
  map(~strsplit(.x, "") %>% unlist) %>%
  do.call(rbind, .)
sea_monster[sea_monster == " "] <- NA

picx <- picture %>% flip %>% rotate # correct rotation/flip trial and error
total_monsters <- 0
for(i in seq(1, 1 + nrow(picx) - nrow(sea_monster))) {
  for(j in seq(1, 1 + ncol(picx) - ncol(sea_monster))) {
    test <- sea_monster == picx[seq(i,nrow(sea_monster)+i-1),seq(j,ncol(sea_monster)+j-1)]
    detect <- na.omit(as.vector(test)) %>% all
    if(detect) {
      cat(i, j, "\n")
      total_monsters <- total_monsters + 1
    }
  }
}
sm_area <- sea_monster %>% as.vector %>% na.omit %>% length
sum(picx == "#") - sm_area * total_monsters
# 2023
