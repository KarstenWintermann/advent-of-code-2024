library(tidyverse)
library(readr)

codes <- str_split(read_file("2024-21.txt"), "\r\n")[[1]]

p <- new.env()
p[["0"]] <- list(4, 2)
p[["A"]] <- list(4, 3)
p[["1"]] <- list(3, 1)
p[["2"]] <- list(3, 2)
p[["3"]] <- list(3, 3)
p[["4"]] <- list(2, 1)
p[["5"]] <- list(2, 2)
p[["6"]] <- list(2, 3)
p[["7"]] <- list(1, 1)
p[["8"]] <- list(1, 2)
p[["9"]] <- list(1, 3)

move_numpad1 <- function(a, b) {
  steps <- ""
  row <- p[[a]][[1]]
  row_2 <- p[[b]][[1]]
  col <- p[[a]][[2]]
  col_2 <- p[[b]][[2]]
  # avoid (4,1)
  if ((row == 4 & col_2 == 1) | (col < col_2 & col != 1)) {
    while (row > row_2) {
      steps <- paste(steps, "^", sep="")
      row <- row - 1
    }
    while (row < row_2) {
      steps <- paste(steps, "v", sep="")
      row <- row + 1
    }
  }
  while (col < col_2) {
    steps <- paste(steps, ">", sep="")
    col <- col + 1
  }
  while (col > col_2) {
    steps <- paste(steps, "<", sep="")
    col <- col - 1
  }
  while (row < row_2) {
    steps <- paste(steps, "v", sep="")
    row <- row + 1
  }
  while (row > row_2) {
    steps <- paste(steps, "^", sep="")
    row <- row - 1
  }
  steps
}

move_numpad <- function(s) {
  steps <- ""
  s1 <- paste("A", s, sep="")
  for (i in seq(from=1, to=str_length(s1)-1)) {
    steps <- 
      paste(
        steps, 
        move_numpad1(
          substring(s1, i, i), 
          substring(s1, i+1, i+1)),
    "A", sep="")
  }
  steps
}

q <- new.env()
q[["^"]] <- list(1, 2)
q[["A"]] <- list(1, 3)
q[["<"]] <- list(2, 1)
q[["v"]] <- list(2, 2)
q[[">"]] <- list(2, 3)

move_dirpad1 <- function(a, b) {
  steps <- ""
  row <- q[[a]][[1]]
  row_2 <- q[[b]][[1]]
  col <- q[[a]][[2]]
  col_2 <- q[[b]][[2]]
  # avoid (1,1)
  if ((row == 1 & col_2 == 1) | (col < col_2 & col != 1)) {
    while (row < row_2) {
      steps <- paste(steps, "v", sep="")
      row <- row + 1
    }
    while (row > row_2) {
      steps <- paste(steps, "^", sep="")
      row <- row - 1
    }
  }
  while (col < col_2) {
    steps <- paste(steps, ">", sep="")
    col <- col + 1
  }
  while (col > col_2) {
    steps <- paste(steps, "<", sep="")
    col <- col - 1
  }
  while (row < row_2) {
    steps <- paste(steps, "v", sep="")
    row <- row + 1
  }
  while (row > row_2) {
    steps <- paste(steps, "^", sep="")
    row <- row - 1
  }
  steps
}

move_dirpad <- function(s) {
  steps <- ""
  s1 <- paste("A", s, sep="")
  for (i in seq(from=1, to=str_length(s1)-1)) {
    steps <- 
      paste(
        steps, 
        move_dirpad1(
          substring(s1, i, i), 
          substring(s1, i+1, i+1)),
        "A", sep="")
  }
  steps
}

sum <- 0
for (i in seq_along(codes)) {
#  print(paste(codes[[i]], move_dirpad(move_dirpad(move_numpad(codes[[i]]))), str_length(move_dirpad(move_dirpad(move_numpad(codes[[i]]))))))
  sum <- sum + str_length(move_dirpad(move_dirpad(move_numpad(codes[[i]])))) * as.numeric(substring(codes[[i]], 1, 3))
}
sum

