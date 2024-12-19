library(tidyverse)
library(readr)
library(rlang)

sections <- str_split(read_file("2024-19test.txt"), "\r\n\r\n")[[1]]
towels <- str_split(str_remove_all(sections[[1]], " "), ",")[[1]]
designs <- str_split(sections[[2]], "\r\n")[[1]]

shortest <- Inf
longest <- 0

towels_env <- new.env()

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
  if (design == "") {
    return(TRUE)
  }
  for (piece in seq(from=shortest, to=longest)) {
    if (towels_env[[substr(design, 1, piece)]] == TRUE) {
      if (find_design(substr(design, piece +1, nchar(design))) == TRUE) {
        return(TRUE)
      }
    }
  }
}

score <- 0
for (d in seq_along(designs)) {
  if (find_design(designs[[d]])) {
    score <- score + 1
  }
}
score