library(tidyverse)
library(readr)
library(rlang)
library(bit64)

sections <- str_split(read_file("2024-19.txt"), "\r\n\r\n")[[1]]
towels <- str_split(str_remove_all(sections[[1]], " "), ",")[[1]]
designs <- str_split(sections[[2]], "\r\n")[[1]]

shortest <- Inf
longest <- 0

towels_env <- new.env()
matched_designs <- new.env()

for (i in seq_along(towels)) {
  len <- nchar(towels[[i]])
  towels_env[[towels[[i]]]] <- TRUE
  if (len < shortest) {
    shortest <<- len
  }
  if (len > longest) {
    longest <<- len
  }
}

find_design <- function(design) {
  len <- nchar(design)
  if (len == 0) {
    return(TRUE)
  }
  if (env_has(matched_designs, design)) {
    return (matched_designs[[design]])
  }
  for (piece in seq(from=longest, to=shortest)) {
    if (len >= piece & env_has(towels_env, substr(design, 1, piece))) {
      if (find_design(substr(design, piece + 1, len)) == TRUE) {
        matched_designs[[design]] <<- TRUE
        return(TRUE)
      }
    }
  }
  matched_designs[[design]] <<- FALSE
  return(FALSE)
}

score <- 0
for (d in seq_along(designs)) {
  print(designs[[d]])
  if (find_design(designs[[d]])) {
    score <- score + 1
  }
}
score

counted_designs <- new.env()
count_designs <- function(design) {
  len <- nchar(design)
  if (len == 0) {
    return(1)
  }
  if (env_has(counted_designs, design)) {
    return (counted_designs[[design]])
  }
  count <- as.integer64(0)
  for (piece in seq(from=longest, to=shortest)) {
    if (len >= piece & env_has(towels_env, substr(design, 1, piece))) {
      count <- count + count_designs(substr(design, piece + 1, len)) 
    }
  }
   
  counted_designs[[design]] <<- count
  return(count)
}

score <- as.integer64(0)
for (d in seq_along(designs)) {
  print(designs[[d]])
  score <- score + count_designs(designs[[d]])
}
score
