library(tidyverse)
library(readr)

options(expressions=20000)

maze <- 
  str_split(
    str_split(
      read_file("2024-16.txt"), 
      "\r\n")[[1]],
    ""
  )

locate_start <- function(symbol) {
  for (row in seq_along(maze)) {
    for (col in seq_along(maze[[row]]))
      if (maze[[row]][[col]] == "S") {
        return (list(row = row, col = col))
      }
  }
}

rotate_cw <- function(heading) {
  if (heading[[2]] == 0) {
    if (heading[[1]] == 1) {
      return(list(0, -1))
    } else {
      return(list(0, 1))
    }
  } else {
    if (heading[[2]] == 1) {
      return(list(1, 0))
    } else {
      return(list(-1, 0))
    }
  }
}

rotate_ccw <- function(heading) {
  if (heading[[2]] == 0) {
    if (heading[[1]] == 1) {
      return(list(0, 1))
    } else {
      return(list(0, -1))
    }
  } else {
    if (heading[[2]] == 1) {
      return(list(-1, 0))
    } else {
      return(list(1, 0))
    }
  }
}

best_steps <- new.env()

find_path <- function(steps, pos, heading) {
  new_pos <- list(pos[[1]] + heading[[1]], pos[[2]] + heading[[2]])
  
#  print(paste(steps, new_pos[[1]], new_pos[[2]], heading[[1]], heading[[2]], maze[[new_pos[[1]]]][[new_pos[[2]]]]))
  
  if (maze[[new_pos[[1]]]][[new_pos[[2]]]] == "E") {
    return(steps+1)
  }
  if (maze[[new_pos[[1]]]][[new_pos[[2]]]] == "#") {
    return(.Machine$integer.max)
  }
  
  key <<- paste(new_pos[[1]], new_pos[[2]], heading[[1]], heading[[2]])
  if (exists(key, best_steps)) {
    if (best_steps[[key]] <= steps) {
      return(.Machine$integer.max)
    }
  }
  
  best_steps[[key]] <<- steps
  
  heading_left <- rotate_ccw(heading)
  heading_right <- rotate_cw(heading)
  
  while (maze[[new_pos[[1]] + heading[[1]]]][[new_pos[[2]] + heading[[2]]]] == "." &
         maze[[new_pos[[1]] + heading_left[[1]]]][[new_pos[[2]] + heading_left[[2]]]] == "#" &
         maze[[new_pos[[1]] + heading_right[[1]]]][[new_pos[[2]] + heading_right[[2]]]] == "#") {
    steps <- steps + 1
    new_pos <- list(new_pos[[1]] + heading[[1]], new_pos[[2]] + heading[[2]])
    key <<- paste(new_pos[[1]], new_pos[[2]], heading[[1]], heading[[2]])
    if (exists(key, best_steps)) {
      if (best_steps[[key]] <= steps) {
        return(.Machine$integer.max)
      }
    }
    best_steps[[key]] <<- steps
  }
  
  print(Cstack_info())
  
  return(min(
    Tailcall(find_path, steps + 1, new_pos, heading),
    Tailcall(find_path, steps + 1001, new_pos, heading_right),
    Tailcall(find_path, steps + 1001, new_pos, heading_left)
  ))
}

min(
  find_path(0, locate_start(), list(row = 0, col = 1)),
  find_path(1000, locate_start(), rotate_cw(list(row = 0, col = 1))),
  find_path(1000, locate_start(), rotate_ccw(list(row = 0, col = 1))))


