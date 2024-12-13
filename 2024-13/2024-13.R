library(tidyverse)
library(matlib)
library(readr)
library(bit64)

lines <- str_split(read_file("2024-13.txt"), "\r\n\r\n")[[1]]

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
  
  if (!str_detect(count_a, "\\.") & !str_detect(count_b, "\\.")) {
    count_a <- strtoi(count_a)
    count_b <- strtoi(count_b)
    if (count_a <= 100 & count_b <= 100) {
      sum <- sum + strtoi(count_a) * 3 + strtoi(count_b)  
    }
  }
}

sum

sum <- as.integer64(0)

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
  prize_x <- as.integer64("10000000000000") + as.integer64(m[[2]])
  prize_y <- as.integer64("10000000000000") + as.integer64(m[[3]])
  
  # do it directly, because matlib.Solve doesn't handle integer64
  count_a <- (b_x * (-prize_y) - b_y * (-prize_x)) / (a_x * b_y - a_y * b_x)
  count_b <- ((-prize_x) * a_y - (-prize_y) * a_x) / (a_x * b_y - a_y * b_x)
    
  if (!str_detect(as.character(count_a), "\\.") & !str_detect(as.character(count_b), "\\.")) {
    sum <- sum + as.integer64(count_a) * 3 + as.integer64(count_b)
  }
}

sum