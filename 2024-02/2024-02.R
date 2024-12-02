library(tidyverse)
library(readr)

rows <- read_delim(read_file("2024-02.txt"), delim = ".", col_names = FALSE, col_types="c")

is_valid_report <- function(report) {
  valid_report <- TRUE
  descending <- (report[[1]] > report[[2]])
  for (column in seq_len(length(report) - 1)) {
    diff <- report[[column+1]] - report[[column]]
    if (descending) {
      if (diff > -1 | diff < -3) {
        valid_report <- FALSE
      }
    } else {
      if (diff < 1 | diff > 3) {
        valid_report <- FALSE
      }
    }
  }
  valid_report
}

find_valid_reports <- function(x) {
  ret <- vector("logical", 1)
  
  for (row in seq_along(x)) {
    line <- sapply(strsplit(x[[row]], ' '), as.integer)
    
    ret[[row]] <- is_valid_report(line)
  }
  
  ret
}

rows |> 
  mutate(valid_report = find_valid_reports(X1)) |>
  summarise(valid_reports = sum(valid_report))

find_valid_reports_dampened <- function(x) {
  ret <- vector("logical", 1)
  
  for (row in seq_along(x)) {
    line <- sapply(strsplit(x[[row]], ' '), as.integer)
    
    valid_line <- FALSE
    for (column in seq_along(line)) {
      if (is_valid_report(line[-c(column)])) {
        valid_line <- TRUE
      }
    }
    
    ret[[row]] <- valid_line
  }
  
  ret
}

rows |> 
  mutate(valid_report = find_valid_reports_dampened(X1)) |>
  summarise(valid_reports_dampened = sum(valid_report))
