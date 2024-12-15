library(tidyverse)
library(readr)
  
sections <- str_split(read_file("2024-15.txt"), "\r\n\r\n")[[1]]

warehouse <- str_split(str_split(sections[[1]], "\r\n")[[1]], "")
instructions <- str_split(str_replace_all(sections[[2]], "\r\n", ""), "")[[1]]

locate_robot <- function(w) {
  for (row in seq_along(w)) {
    for (col in seq_along(w[[row]]))
      if (w[[row]][[col]] == "@") {
        return (list(row, col))
      }
  }
}

make_space_and_move <- function(p, row_vec, col_vec) {
  new_p <- list(p[[1]] + row_vec, p[[2]] + col_vec)
  if (warehouse[[new_p[[1]]]][[new_p[[2]]]] == ".") {
    return(new_p)
  }
  if (warehouse[[new_p[[1]]]][[new_p[[2]]]] == "#") {
    return(p)
  }
  free_p <- new_p
  while (warehouse[[free_p[[1]]]][[free_p[[2]]]] != ".") {
    if (warehouse[[free_p[[1]]]][[free_p[[2]]]] == "#") {
      return(p)
    }
    free_p <- list(free_p[[1]] + row_vec, free_p[[2]] + col_vec)
  }
  warehouse[[free_p[[1]]]][[free_p[[2]]]] <<- "O"
  warehouse[[new_p[[1]]]][[new_p[[2]]]]  <<- "."
  return(new_p)
}

r <- locate_robot(warehouse)
warehouse[[r[[1]]]][[r[[2]]]] <- "."
for (i in seq_along(instructions)) {
  if (instructions[[i]] == "^") {
    r <- make_space_and_move(r, -1, 0)
  }
  if (instructions[[i]] == "v") {
    r <- make_space_and_move(r, 1, 0)
  }
  if (instructions[[i]] == "<") {
    r <- make_space_and_move(r, 0, -1)
  }
  if (instructions[[i]] == ">") {
    r <- make_space_and_move(r, 0, 1)
  }
}

score <- 0
for (row in seq_along(warehouse)) {
  for (col in seq_along(warehouse[[row]]))
    if (warehouse[[row]][[col]] == "O") {
      score <- score + (row - 1) * 100 + (col - 1)
    }
}
score

warehouse2 <- 
  str_replace_all(sections[[1]], "#", "##") |>
  str_replace_all("\\.", "\\.\\.") |>
  str_replace_all("O", "[]") |>
  str_replace_all("@", "@.") |>
  str_split("\r\n")

warehouse2 <- str_split(warehouse2[[1]], "")

test_space_and_move <- function(p, row_vec, col_vec) {
  new_p <- list(p[[1]] + row_vec, p[[2]] + col_vec)
  if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == ".") {
    return(TRUE)
  } else if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "#") {
    return(FALSE)
  } else {
    if (row_vec != 0) {
      if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "[") {
        return(
          test_space_and_move(new_p, row_vec, col_vec) &
          test_space_and_move(list(new_p[[1]], new_p[[2]]+1), row_vec, col_vec))
      }
      if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "]") {
        return(
          test_space_and_move(new_p, row_vec, col_vec) &
          test_space_and_move(list(new_p[[1]], new_p[[2]]-1), row_vec, col_vec))
      }
    }
    return(test_space_and_move(new_p, row_vec, col_vec))
  }
}

make_space_and_move2 <- function(p, row_vec, col_vec) {
  new_p <- list(p[[1]] + row_vec, p[[2]] + col_vec)
  if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == ".") {
    warehouse2[[new_p[[1]]]][[new_p[[2]]]] <<- warehouse2[[p[[1]]]][[p[[2]]]]
    warehouse2[[p[[1]]]][[p[[2]]]] <<- "."
    return(new_p)
  } else if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "#") {
    return(p)
  } else {
    if (row_vec != 0) {
      if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "[") {
        make_space_and_move2(new_p, row_vec, col_vec) 
        make_space_and_move2(list(new_p[[1]], new_p[[2]]+1), row_vec, col_vec)
      }
      if (warehouse2[[new_p[[1]]]][[new_p[[2]]]] == "]") {
        make_space_and_move2(new_p, row_vec, col_vec) 
        make_space_and_move2(list(new_p[[1]], new_p[[2]]-1), row_vec, col_vec)
      }
    } else {
      make_space_and_move2(new_p, row_vec, col_vec)
    }
    warehouse2[[new_p[[1]]]][[new_p[[2]]]] <<- warehouse2[[p[[1]]]][[p[[2]]]]
    warehouse2[[p[[1]]]][[p[[2]]]] <<- "."
    return(new_p)
  }
}

r <- locate_robot(warehouse2)
warehouse2[[r[[1]]]][[r[[2]]]] <- "."
for (i in seq_along(instructions)) {
  if (instructions[[i]] == "^") {
    if (test_space_and_move(r, -1, 0)) {
      r <- make_space_and_move2(r, -1, 0)
    }
  }
  if (instructions[[i]] == "v") {
    if (test_space_and_move(r, 1, 0)) {
      r <- make_space_and_move2(r, 1, 0)
    }
  }
  if (instructions[[i]] == "<") {
    if (test_space_and_move(r, 0, -1)) {
      r <- make_space_and_move2(r, 0, -1)
    }
  }
  if (instructions[[i]] == ">") {
    if (test_space_and_move(r, 0, 1)) {
      r <- make_space_and_move2(r, 0, 1)
    }
  }
}

score <- 0
for (row in seq_along(warehouse2)) {
  for (col in seq_along(warehouse2[[row]]))
    if (warehouse2[[row]][[col]] == "[") {
      score <- score + (row - 1) * 100 + (col - 1)
    }
}
score
