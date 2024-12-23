library(tidyverse)
library(readr)
library(bit64)

cycle <- function(n) {
  n <- bitwXor(n, bitwShiftL(n, 6)) %% 16777216
  n <- bitwXor(n, bitwShiftR(n, 5)) %% 16777216
  n <- bitwXor(n, bitwShiftL(n, 11)) %% 16777216
}

numbers <- strtoi(str_split(read_file("2024-22.txt"), "\r\n")[[1]])

sum <- as.integer64(0)
for (i in seq_along(numbers)) {
  n <- numbers[[i]]
  for (j in seq(from=1, to=2000)) {
    n <- cycle(n)
  }
  sum <- sum + n
}
sum