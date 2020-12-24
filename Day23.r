library(dplyr)

# input <- "389125467" %>% strsplit("") %>% unlist %>% as.numeric
input <- "362981754" %>% strsplit("") %>% unlist %>% as.numeric

# Part 1
do_move <- function(input, i) {
  current <- input[i]
  pickup_idx <- (seq(i, i+2) %% length(input)) + 1
  pickup <- input[pickup_idx]
  input <- input[-pickup_idx]
  next_index_val <- input[(which(input == current) %% length(input)) + 1]
  diff <- current - input
  diff <- ifelse(diff <= 0, Inf, diff)
  dest_idx <- ifelse(any(is.finite(diff)), which.min(diff), which.max(input))
  input <- c(input[seq(1,dest_idx)], pickup, 
             input[if(dest_idx < length(input)) seq(dest_idx+1,length(input)) else integer(0)]
  )
  next_i <- which(input == next_index_val)
  return(list(input, next_i))
}

i <- 1
for(q in 1:100) {
  ret <- do_move(input, i)
  input <- ret[[1]]
  i <- ret[[2]]
  cat(i, ":", input, "\n")
}
w <- which(input == 1)
if(w == length(input)) {
  paste0(input[-w], collapse = "")
} else {
  c(input[seq(w+1,length(input))], input[seq(1,w-1)]) %>%
    paste0(collapse = "")
}
# [1] "24798635"

# Part 2

# R6 class is conceptually correct but there is too much overhead >:(

# library(R6)
# LinkedList <- R6Class("LinkedList",
#   public = list(
#     sv = NULL,
#     pv = NULL,
#     initialize = function(x) {
#       s <- integer(length(x))
#       p <- integer(length(x))
#       for(i in 1:length(x)) {
#         if(i == length(x)) {
#           s[x[i]] <- x[1]
#         } else {
#           s[x[i]] <- x[i+1]
#         }
#         if(i == 1) {
#           p[x[i]] <- x[length(x)]
#         } else {
#           p[x[i]] <- x[i-1]
#         }
#       }
#       self$sv <- s
#       self$pv <- p
#     },
#     get_successor = function(x) {
#       self$sv[x]
#     },
#     get_predecessor = function(x) {
#       self$pv[x]
#     },
#     set_successor = function(x,s) {
#       self$sv[x] <- s
#       self$pv[s] <- x
#     },
#     set_predecessor = function(x,p) {
#       self$pv[x] <- p
#       self$sv[p] <- x
#     },
#     write = function(origin = 1) {
#       out <- integer(length=length(self$sv))
#       x <- origin
#       for(i in 1:length(self$sv)) {
#         out[i] <- x
#         x <- self$sv[x]
#       }
#       check <- integer(length=length(self$pv))
#       x <- tail(out, 1)
#       for(i in length(self$pv):1) {
#         check[i] <- x
#         x <- self$pv[x]
#       }
#       stopifnot(identical(out, check))
#       out
#     },
#     length = function() {
#       length(self$sv)
#     }
#   )
# )

get_linked_list <- function(x) {
  s <- integer(length(x))
  p <- integer(length(x))
  for(i in 1:length(x)) {
    if(i == length(x)) {
      s[x[i]] <- x[1]
    } else {
      s[x[i]] <- x[i+1]
    }
    if(i == 1) {
      p[x[i]] <- x[length(x)]
    } else {
      p[x[i]] <- x[i-1]
    }
  }
  return(list(s,p))
}

get_successor = function(x) {
  sv[x]
}
get_predecessor = function(x) {
  pv[x]
}

set_successor = function(x,s) {
  sv[x] <<- s
  pv[s] <<- x
}
set_predecessor = function(x,p) {
  pv[x] <<- p
  sv[p] <<- x
}

write = function(origin = 1) {
  out <- integer(length=length(sv))
  x <- origin
  for(i in 1:length(sv)) {
    out[i] <- x
    x <- sv[x]
  }
  check <- integer(length=length(pv))
  x <- tail(out, 1)
  for(i in length(pv):1) {
    check[i] <- x
    x <- pv[x]
  }
  stopifnot(identical(out, check))
  out
}


# input <- "389125467" %>% strsplit("") %>% unlist %>% as.integer
input <- "362981754" %>% strsplit("") %>% unlist %>% as.numeric
input <- c(input, seq(10L,1e6L))
linked <- get_linked_list(input)
sv <- linked[[1]]
pv <- linked[[2]]
rm(linked)

x <- 3
for(q in 1:1e7) {
  # temp <- Sys.time()
  if(q %% 100000 == 0) print(q)
  s1 <- get_successor(x)
  s2 <- get_successor(s1)
  s3 <- get_successor(s2)
  pickup <- c(s1, s2, s3)
  s4 <- get_successor(s3)
  set_successor(x,s4)
  # time1 <- as.numeric(Sys.time() - temp, units="secs") + time1
  
  # temp <- Sys.time()
  dest <- NA
  for(i in seq(x-1,x-4)) {
    if(i <= 0) break
    if(!i %in% pickup) {
      dest <- i
      break
    }
  }
  if(is.na(dest)) {
    for(i in seq(length(sv), length(sv)-4 ) ) {
      if(!i %in% pickup) {
        dest <- i
        break
      }
    }
  }
  # time2 <- as.numeric(Sys.time() - temp, units="secs") + time2
  
  # temp <- Sys.time()
  dest_successor <- get_successor(dest)
  set_successor(dest,s1)
  set_successor(s3,dest_successor)
  x <- get_successor(x)
  # time3 <- as.numeric(Sys.time() - temp, units="secs") + time3
}

r1 <- get_successor(1)
r2 <- get_successor(r1)
print(r1*r2, digits=22)

# 12757828710