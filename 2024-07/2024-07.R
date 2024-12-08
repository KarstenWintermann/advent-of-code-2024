library(tidyverse)
library(readr)
library(bit64)

equations <- read_file("2024-07.txt") 

lines <- str_split(equations, "\r\n")

permutate <- function(res, left_res, ops) {
  if (length(ops) == 0) {
    return (left_res == res) 
  }
  else {
    val <- as.integer64(strtoi(head(ops, 1)))
    p <- permutate(res, left_res + val, tail(ops, length(ops) - 1)) 
    if (p == TRUE) {
      return(TRUE)
    }
    if (left_res == 0) {
      left_res <- as.integer64(1)
    }
    p <- permutate(res, left_res * val, tail(ops, length(ops) - 1)) 
    if (p == TRUE) {
      return(TRUE) 
    }
  }
  return(FALSE)
}

total <- as.integer64(0)
for (line in seq_along(lines[[1]])) {
  leftright <- str_split(lines[[1]][[line]], ": ")
  res <- as.integer64(leftright[[1]][[1]])
  ops <- str_split(leftright[[1]][[2]], " ")
  if (permutate(res, as.integer64(0), ops[[1]])) {
    total <- total + res
  }
}

total

total <- as.integer64(0)

permutate2 <- function(res, left_res, ops) {
  if (length(ops) == 0) {
    return (left_res == res) 
  }
  else {
    val <- as.integer64(strtoi(head(ops, 1)))
    p <- permutate2(res, left_res + val, tail(ops, length(ops) - 1)) 
    if (p == TRUE) {
      return(TRUE)
    }
    if (left_res == 0) {
      left_res <- as.integer64(1)
    }
    p <- permutate2(res, left_res * val, tail(ops, length(ops) - 1)) 
    if (p == TRUE) {
      return(TRUE) 
    }
    if (left_res != 0) {
      p <- permutate2(res, as.integer64(paste(left_res, val, sep="")), tail(ops, length(ops) - 1)) 
      if (p == TRUE) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

total <- as.integer64(0)
for (line in seq_along(lines[[1]])) {
  leftright <- str_split(lines[[1]][[line]], ": ")
  res <- as.integer64(leftright[[1]][[1]])
  ops <- str_split(leftright[[1]][[2]], " ")
  if (permutate2(res, as.integer64(0), ops[[1]])) {
    total <- total + res
  }
}

total