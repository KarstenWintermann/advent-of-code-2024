library(tidyverse)
library(readr)

filename <- "2024-10.txt"

topology <- 
  str_split(
    str_split(
      read_file(filename), 
      "\r\n")[[1]], 
    "")

for (row in seq_along(topology)) {
  topology[[row]] <- strtoi(topology[[row]])
}

in_topology <- function(row, col) {
  row > 0 &
  col > 0 & 
  row <= length(topology) &
  col <= length(topology[[1]])
}

find_nines <- function(row, col, level) {
  if (!in_topology(row, col)) {
    return (c())
  }
  newlevel <- topology[[row]][[col]]
  if (newlevel != level) {
    return (c())
  }
  if (newlevel == 9) {
    return (row + 1000 * col)
  } 
  return (c(find_nines(row+1, col, level+1),
            find_nines(row-1, col, level+1),
            find_nines(row, col+1, level+1),
            find_nines(row, col-1, level+1)))
}

trailhead_score <- 0
for (row in seq_along(topology)) {
  for (col in seq_along(topology[[row]])) {
    if (topology[[row]][[col]] == 0) {
      trailhead_score <- trailhead_score + length(unique(c(find_nines(row, col, 0))))
    }
  }
}

trailhead_score

trailhead_score <- 0
for (row in seq_along(topology)) {
  for (col in seq_along(topology[[row]])) {
    if (topology[[row]][[col]] == 0) {
      trailhead_score <- trailhead_score + length(c(find_nines(row, col, 0)))
    }
  }
}

trailhead_score