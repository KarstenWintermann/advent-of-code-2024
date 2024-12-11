library(tidyverse)
library(readr)
library(bit64)

line <- strtoi(str_split(read_file("2024-11.txt"), " ")[[1]])

already_counted <- new.env()

count_stones <- function(stone, blinks_left) {
  blinks_left <- blinks_left - 1
  
  key <- paste(stone, blinks_left)
  if (exists(key, already_counted)) {
    return(already_counted[[key]])
  }
  
  res <- 0
  if (blinks_left + 1 == 0) {
    res <- 1
  }
  else if (stone == 0) {
    res <- count_stones(1, blinks_left)
  }
  else {
    stone_str <- as.character(stone)
    len <- str_length(stone_str)
    if (len %% 2 == 0) {
      res <- 
        count_stones(as.integer64(substring(stone_str, 1, len/2)), blinks_left) + 
        count_stones(as.integer64(substring(stone_str, len/2+1, len)), blinks_left)
    } else {
      res <- count_stones(stone * 2024, blinks_left)
    }
  }
  
  already_counted[[key]] <- res
  
  return(res)
}

ptm <- proc.time()
stones <- as.integer64(0)
for (num in seq_along(line)) {
  stones <- stones + count_stones(as.integer64(line[[num]]), 25)
}
proc.time() - ptm
stones

ptm <- proc.time()
stones <- as.integer64(0)
for (num in seq_along(line)) {
  stones <- stones + count_stones(as.integer64(line[[num]]), 75)
}
proc.time() - ptm
stones