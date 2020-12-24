library(dplyr)
library(data.table)
library(parallel)

setwd("~/GoogleDrive/advent_of_code_solutions/")

input <- readLines("Data/Day24.txt")

# e --> e
# w --> w
# ne --> N
# se --> S
# nw --> M
# sw --> R

input <- gsub("ne", "N", input) %>%
  gsub("se", "S", .) %>%
  gsub("nw", "M", .) %>%
  gsub("sw", "R", .)

table_vec <- function(x) {
  x <- table(x)
  as.vector(x)
  n <- names(x)
  x <- as.vector(x)
  names(x) <- n
  x
}

input <- strsplit(input,"")

# Day 1
levels <- c("N", "e", "S", "R", "w", "M")
res <- lapply(input, function(x) {
  x <- table_vec(factor(x, levels = levels))
  data.frame(X1 = x["N"] - x["R"], X2 = x["e"] - x["w"], X3 = x["S"] - x["M"])
}) %>% do.call(rbind, .)

res <- data.frame(res)
res <- res %>% 
  mutate(X = X2 + 0.5*X1 + 0.5*X3,
         Y = (X1-X3) * sqrt(3)/2)

res <- res %>% group_by(X,Y) %>%
  summarize(n = n()) %>% filter(n %% 2 == 1)

nrow(res)
# 388

# Day 2
levels <- c("N", "e", "S", "R", "w", "M")
res <- lapply(input, function(x) {
  x <- table_vec(factor(x, levels = levels))
  data.frame(X1 = x["N"] - x["R"], X2 = x["e"] - x["w"], X3 = x["S"] - x["M"])
}) %>% do.call(rbind, .)

res <- data.frame(res)
res <- res %>% 
  mutate(x = 2*X2 + X1 + X3, y = X1-X3) %>%
  group_by(x,y) %>%
  summarize(n = n()) %>%
  filter(n %% 2 == 1) %>%
  dplyr::select(-n)

# Neighbors
# +2X, +0Y
# +/-1X, +/-1Y
get_neighbors <- function(x,y) {
  data.frame(x=c(x+1,x+2,x+1,x-1,x-2,x-1),
             y=c(y+1,y  ,y-1,y-1,y  ,y+1))
}

time <- Sys.time()
active <- res
for(. in 1:100) {
  active_str <- paste0(active$x, ":", active$y)
  nbs_ia <- mclapply(1:nrow(active), function(i) {
    get_neighbors(active$x[i], active$y[i])
  }, mc.cores=8) %>% rbindlist %>% distinct
  nbs_ia <- anti_join(nbs_ia, active, by = c("x", "y"))

  update_ia <- mclapply(1:nrow(nbs_ia), function(i) {
    z <- get_neighbors(nbs_ia$x[i], nbs_ia$y[i])
    z <- paste0(z$x, ":", z$y)
    sum(z %in% active_str) == 2
  }, mc.cores=8) %>% unlist

  update_a <- mclapply(1:nrow(active), function(i) {
    z <- get_neighbors(active$x[i], active$y[i])
    z <- paste0(z$x, ":", z$y)
    sz <- sum(z %in% active_str)
    sz != 0 && sz <= 2
  }, mc.cores=8) %>% unlist
  active <- rbind( active[update_a,], nbs_ia[update_ia,] )
  cat(., nrow(active), "\n")
}
print(Sys.time() - time)

# 4002
