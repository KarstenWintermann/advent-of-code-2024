library(tidyverse)
library(readr)

track <- str_split(str_split(read_file("2024-20test.txt"), "\r\n")[[1]], "")

dist <- array(Inf, c(length(track), length(track[[1]])))

find_start <- function() {
  for (row in seq_along(track)) {
    for (col in seq_along(track[[row]])) {
      if (track[[row]][[col]] == "S") {
        return(list(row, col))
      }
    }
  }
  return(list(0, 0))
}

test_next_pos <- function(row, col) {
  track[[row]][[col]] != "#" & dist[[row, col]] == Inf
}

mark_dist <- function() {
  pos <- find_start()
  row <- pos[[1]]
  col <- pos[[2]]
  picoseconds <- 0
  while (track[[row]][[col]] != "E") {
    dist[[row, col]] <<- picoseconds
    if (test_next_pos(row, col+1)) {
      col <- col + 1
    }
    else if (test_next_pos(row, col-1)) {
      col <- col - 1
    }
    else if (test_next_pos(row+1, col)) {
      row <- row + 1
    }
    else if (test_next_pos(row-1, col)) {
      row <- row -1
    }
    picoseconds <- picoseconds + 1
  }
  dist[[row, col]] <<- picoseconds
}

mark_dist()

find_gain <- function(row1, col1, row2, col2) {
  gain <- (dist[[row1, col1]] - dist[[row2, col2]])
  if (is.nan(gain)) {
    return(0)
  }
  if (gain == Inf | gain == -Inf) {
    return(0)
  }
  return(gain)
}

find_cheats <- function(threshold) {
  found <- 0
  for (row in seq(from=2, to=length(track)-1))  {
    for (col in seq(from=2, to=length(track[[row]])-1)) {
      if (track[[row]][[col]] == "#") {
        if (find_gain(row, col+1, row, col-1) -2 >= threshold) {
          found <- found + 1
        }
        if (find_gain(row, col-1, row, col+1) -2 >= threshold) {
          found <- found + 1
        }
        if (find_gain(row+1, col, row-1, col) -2 >= threshold) {
          found <- found + 1
        }
        if (find_gain(row-1, col, row+1, col) -2 >= threshold) {
          found <- found + 1
        }
      }
    }
  }
  found
}

find_cheats(100)

valid_end <- function(row, col) {
  if (row < 1 | row > length(track) | col < 1 | col > length(track[[1]])) {
    return(FALSE)
  }
  if (track[[row]][[col]] == "#") {
    return(FALSE)
  }
  return(TRUE)
}

cheat_distance <- function(row, col, row2, col2) {
  return(abs(row2-row) + abs(col2-col))
}

find_cheats_around <- function(row, col, threshold) {
  found <- 0
  for (row2 in seq(from=row-20, to=row+20)) {
    n_cols <- 20 - abs(row - row2)
    for (col2 in seq(from=col - n_cols, to=col + n_cols)) {
      if (valid_end(row2, col2)) {
        if (find_gain(row, col, row2, col2) - cheat_distance(row, col, row2, col2) >= threshold) {
          found <- found + 1       
        }
      }
    } 
  }
  found
}

find_cheats2 <- function(threshold) {
  found <- 0
  for (row in seq(from=2, to=length(track)-1))  {
    for (col in seq(from=2, to=length(track[[row]])-1)) {
      print(paste(row, col))
      if (track[[row]][[col]] != "#") {
        found <- found + find_cheats_around(row, col, threshold)
      }
    }
  }
  found
}

find_cheats2(100)