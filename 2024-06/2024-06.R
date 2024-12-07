library(tidyverse)
library(readr)

filename <- "2024-06.txt"

maze <- 
  str_split(
    str_split(
      read_file(filename), 
      "\r\n")[[1]], 
    "")

locate_guard <- function() {
  for (row in seq_along(maze)) {
    for (col in seq_along(maze[[row]])) {
      if (maze[[row]][[col]] == "^") {
        return(list(row, col))
      }
    }
  }  
}

move_guard <- function(guard, heading) {
  if (heading == 0) {
    return(list(guard[[1]]-1, guard[[2]]))
  }
  if (heading == 90) {
    return(list(guard[[1]], guard[[2]]+1))
  }
  if (heading == 180) {
    return(list(guard[[1]]+1, guard[[2]]))
  }
  if (heading == 270) {
    return(list(guard[[1]], guard[[2]]-1))
  }
}

in_maze <- function(guard) {
  return (guard[[1]] > 0 & 
    guard[[1]] <= length(maze) &
    guard[[2]] > 0 &
    guard[[2]] <= length(maze[[1]])) 
}

guard <- locate_guard()
heading <- 0
visited <- 1

while(in_maze(guard)){
  nextpos <- move_guard(guard, heading)
  if (!in_maze(nextpos)) {
    break
  }
  else if (maze[[nextpos[[1]]]][[nextpos[[2]]]] == "#") {
    heading <- heading + 90
    if (heading == 360) {
      heading <- 0
    }
  }
  else if (maze[[nextpos[[1]]]][[nextpos[[2]]]] == ".") {
    guard <- nextpos
    maze[[guard[[1]]]][[guard[[2]]]] = "X"
    visited <- visited + 1
  } 
  else {
    guard <- nextpos
  }
}

visited

test_loop <- function() {
  loop <- FALSE
  visited_0 <- maze
  visited_90 <- maze
  visited_180 <- maze
  visited_270 <- maze
  guard <- locate_guard()
  heading <- 0
  
  while(in_maze(guard)){
    nextpos <- move_guard(guard, heading)
    if (!in_maze(nextpos)) {
      break
    }
    else if (maze[[nextpos[[1]]]][[nextpos[[2]]]] == "#") {
      heading <- heading + 90
      if (heading == 360) {
        heading <- 0
      }
    }
    else {
      guard <- nextpos
      if (heading == 0) {
        if (visited_0[[nextpos[[1]]]][[nextpos[[2]]]] == "X") {
          loop <- TRUE
          break
        } else {
          visited_0[[nextpos[[1]]]][[nextpos[[2]]]] = "X"
        }
      } else if (heading == 90) {
        if (visited_90[[nextpos[[1]]]][[nextpos[[2]]]] == "X") {
          loop <- TRUE
          break
        } else {
          visited_90[[nextpos[[1]]]][[nextpos[[2]]]] = "X"
        }
      } else if (heading == 180) {
        if (visited_180[[nextpos[[1]]]][[nextpos[[2]]]] == "X") {
          loop <- TRUE
          break
        } else {
          visited_180[[nextpos[[1]]]][[nextpos[[2]]]] = "X"
        }
      } else if (heading == 270) {
        if (visited_270[[nextpos[[1]]]][[nextpos[[2]]]] == "X") {
          loop <- TRUE
          break
        } else {
          visited_270[[nextpos[[1]]]][[nextpos[[2]]]] = "X"
        }
      }
      maze[[guard[[1]]]][[guard[[2]]]] = "X"
      visited <- visited + 1
    } 
  }
  
  loop
}

maze <- 
  str_split(
    str_split(
      read_file(filename), 
      "\r\n")[[1]], 
    "")

n_positions <- 0
  
for (row in seq_along(maze)) {
  for (col in seq_along(maze[[row]])) {
    if (maze[[row]][[col]] == ".") {
      maze[[row]][[col]] <- "#"
      if (test_loop()) {
        n_positions <- n_positions + 1
      }
      maze[[row]][[col]] <- "."
    }
  }
}  

n_positions