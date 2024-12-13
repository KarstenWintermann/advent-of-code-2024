library(tidyverse)
library(matlib)
library(readr)

lines <- str_split(read_file("2024-13test.txt"), "\r\n\r\n")[[1]]

sum <- 0

for (line in seq_along(lines)) {
  record <- str_split(lines[[line]], "\r\n")
  r <- regexec("Button A: X\\+([0-9]+), Y\\+([0-9]+)", record[[1]][[1]])
  m <- regmatches(record[[1]][[1]], r)[[1]]
  a_x <- strtoi(m[[2]])
  a_y <- strtoi(m[[3]])

  r <- regexec("Button B: X\\+([0-9]+), Y\\+([0-9]+)", record[[1]][[2]])
  m <- regmatches(record[[1]][[2]], r)[[1]]
  b_x <- strtoi(m[[2]])
  b_y <- strtoi(m[[3]])
  
  r <- regexec("Prize: X=([0-9]+), Y=([0-9]+)", record[[1]][[3]])
  m <- regmatches(record[[1]][[3]], r)[[1]]
  prize_x <- strtoi(m[[2]])
  prize_y <- strtoi(m[[3]])
  
  A <- matrix(c(a_x, a_y, b_x, b_y), 2, 2) 
  b <- c(prize_x, prize_y)
  s <- Solve(A, b)
  
  r <- regexec("x1\\s+=\\s+([0-9.]*)", s[[1]])
  m <- regmatches(s[[1]], r)[[1]]
  count_a <- m[[2]]
  
  r <- regexec("\\s+x2\\s+=\\s+([0-9.]*)", s[[2]])
  m <- regmatches(s[[2]], r)[[1]]
  count_b <- m[[2]]
  
  if (!str_detect(count_a, ".") & !str_detect(count_b, ".")) {
    sum <- strtoi(count_a) * 3 + strtoi(count_b)
  }
}

sum