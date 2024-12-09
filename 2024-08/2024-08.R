library(tidyverse)
library(readr)

filename <- "2024-08.txt"

grid <- 
  str_split(
    str_split(
      read_file(filename), 
      "\r\n")[[1]], 
    "")

in_grid <- function(row, col) {
  res <- (row > 0 & 
          row <= length(grid) &
          col > 0 &
          col <= length(grid[[1]]))
  
  res
}

count_resonance <- 0

mark_grid <- grid

for (row in seq_along(grid)) {
  for (col in seq_along(grid[[row]])) {
    station <- grid[[row]][[col]]
    if (station != "." & station != "#") {
      for (row2 in seq_along(grid)) {
        for (col2 in seq_along(grid[[row2]])) {
          if (row != row2 & 
              col != col2 & 
              grid[[row2]][[col2]] == station) {
            row_resonance = row + (row2 - row) * 2
            col_resonance = col + (col2 - col) * 2
            if (in_grid(row_resonance, col_resonance)) {
              if (mark_grid[[row_resonance]][[col_resonance]] != "#") {
                count_resonance <- count_resonance + 1
                mark_grid[[row_resonance]][[col_resonance]] = "#"
              }
            }
          }
        }
      }  
    }
  }
}

count_resonance

count_resonance <- 0

mark_grid <- grid

for (row in seq_along(grid)) {
  for (col in seq_along(grid[[row]])) {
    station <- grid[[row]][[col]]
    if (station != "." & station != "#") {
      for (row2 in seq_along(grid)) {
        for (col2 in seq_along(grid[[row2]])) {
          if (row != row2 & 
              col != col2 & 
              grid[[row2]][[col2]] == station) {
            factor <- 1
            while(
              in_grid(
                row + (row2 - row) * factor,
                col + (col2 - col) * factor)) {
              if (mark_grid[[row + (row2 - row) * factor]][[col + (col2 - col) * factor]] != "#") {
                count_resonance <- count_resonance + 1
                mark_grid[[row + (row2 - row) * factor]][[col + (col2 - col) * factor]] = "#"
              }
              factor <- factor + 1
            }             
          }
        }
      }  
    }
  }
}

count_resonance