library(tidyverse)
library(readr)

n_rows <- 71
n_cols <- 71
n_bytes <- 1024
filename <- "2024-18.txt"

bytes <- str_split(str_split(read_file(filename), "\r\n")[[1]], ",")
grid <- array(".", c(n_rows, n_cols))

for (byte in seq(from = 1, to = n_bytes)) {
  row <- strtoi(bytes[[byte]][[1]]) + 1
  col <- strtoi(bytes[[byte]][[2]]) + 1
  grid[[row, col]] <- "#"
}

bestPath <- array(Inf, c(n_rows, n_cols))

findPath <- function(p, steps) {
  if (p[[1]] == n_rows & p[[2]] == n_cols) {
    return(steps)
  }
  if (p[[1]] < 1 | p[[1]] > n_rows | p[[2]] < 1 | p[[2]] > n_cols) {
    return(Inf)
  }
  if (grid[[p[[1]], p[[2]]]] == "#") {
    return(Inf)
  }
  if (bestPath[[p[[1]],p[[2]]]] <= steps + 1) {
    return(Inf)
  }
  bestPath[[p[[1]],p[[2]]]] <<- steps + 1
  return(min(
    findPath(list(p[[1]]+1, p[[2]]), steps + 1),
    findPath(list(p[[1]]-1, p[[2]]), steps + 1),
    findPath(list(p[[1]], p[[2]]+1), steps + 1),
    findPath(list(p[[1]], p[[2]]-1), steps + 1)))
}

findPath(list(1,1), 0)

grid <- array(".", c(n_rows, n_cols))

for (byte in seq_along(bytes)) {
  row <- strtoi(bytes[[byte]][[1]]) + 1
  col <- strtoi(bytes[[byte]][[2]]) + 1
  grid[[row, col]] <- "#"

  bestPath <<- array(Inf, c(n_rows, n_cols))
  
  if (findPath(list(1,1), 0) == Inf) {
    print(paste(row-1, col-1, sep=","))
    break
  }
}
