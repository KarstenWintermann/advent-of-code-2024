library(tidyverse)
library(readr)

#lines <- str_split(read_file("2024-14test.txt"), "\r\n")[[1]]
#cols <- 11
#rows <- 7
#seconds <- 100

lines <- str_split(read_file("2024-14.txt"), "\r\n")[[1]]
cols <- 101
rows <- 103
seconds <- 100

quadrant_1 <- 0
quadrant_2 <- 0
quadrant_3 <- 0
quadrant_4 <- 0

for (n in seq_along(lines)) {
  line <- lines[[n]]
  
  r <- regexec("p=([0-9]+),([0-9]+) v=([0-9\\-]+),([0-9\\-]+)", line)
  m <- regmatches(line, r)[[1]]
  start_col <- strtoi(m[[2]])
  start_row <- strtoi(m[[3]])
  vec_col <- strtoi(m[[4]])
  vec_row <- strtoi(m[[5]])
  
  end_col <- (start_col + vec_col * seconds) %% cols
  end_row <- (start_row + vec_row * seconds) %% rows
  
  print(paste(start_row, start_col, vec_row, vec_col, end_row, end_col))
  
  if (end_col < (cols - 1)/2 & end_row < (rows - 1)/2) {
    quadrant_1 <- quadrant_1 + 1
  }
  if (end_col < (cols - 1)/2 & end_row > (rows - 1)/2) {
    quadrant_2 <- quadrant_2 + 1
  }
  if (end_col > (cols - 1)/2 & end_row < (rows - 1)/2) {
    quadrant_3 <- quadrant_3 + 1
  }
  if (end_col > (cols - 1)/2 & end_row > (rows - 1)/2) {
    quadrant_4 <- quadrant_4 + 1
  }
}

print(paste(quadrant_1, quadrant_2, quadrant_3, quadrant_4))

quadrant_1 * quadrant_2 * quadrant_3 * quadrant_4

print_bathroom <- function(bathroom) {
  for (i in 1:(rows-1)) {
    for (j in 1:(cols-1)) {
#      print(paste(i, j))
      b <- bathroom[[i,j]]
      if (b == 0) {
        cat(" ", sep = "")
      } else {
        cat(b, sep = "")  
      }
    }
    cat("\r\n")
  }
}

print_after_seconds <- function(n_seconds) {
  bathroom <- array(0, c(rows, cols))
  
  for (n in seq_along(lines)) {
    line <- lines[[n]]
  
    r <- regexec("p=([0-9]+),([0-9]+) v=([0-9\\-]+),([0-9\\-]+)", line)
    m <- regmatches(line, r)[[1]]
    start_col <- strtoi(m[[2]])
    start_row <- strtoi(m[[3]])
    vec_col <- strtoi(m[[4]])
    vec_row <- strtoi(m[[5]])
  
    end_col <- (start_col + vec_col * n_seconds) %% cols
    end_row <- (start_row + vec_row * n_seconds) %% rows
  
    bathroom[end_row, end_col] <- bathroom[end_row, end_col] + 1
  }
  
  print_bathroom(bathroom)
}

seconds <- 99
while (TRUE) {
  print_after_seconds(seconds)
  invisible(line <- readline(prompt=paste("after", seconds, "seconds")))  
  seconds <- seconds + 103
  if (line == "x") {
    break
  }
}
