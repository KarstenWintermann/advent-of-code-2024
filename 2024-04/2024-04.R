library(tidyverse)
library(readr)

grid <- read_lines(read_file("2024-04.txt"))

line_length <- str_length(grid[[1]])

grid <- paste(grid, collapse = "|")

found <- str_count(grid, "XMAS")
found <- found + str_count(grid, "SAMX")
found <- found + str_count(grid, sprintf("(?=(S.{%d}A.{%d}M.{%d}X))", line_length - 1, line_length - 1, line_length - 1))
found <- found + str_count(grid, sprintf("(?=(X.{%d}M.{%d}A.{%d}S))", line_length - 1, line_length - 1, line_length - 1))

found <- found + str_count(grid, sprintf("(?=(S.{%d}A.{%d}M.{%d}X))", line_length, line_length, line_length))
found <- found + str_count(grid, sprintf("(?=(X.{%d}M.{%d}A.{%d}S))", line_length, line_length, line_length))

found <- found + str_count(grid, sprintf("(?=(S.{%d}A.{%d}M.{%d}X))", line_length + 1, line_length + 1, line_length + 1))
found <- found + str_count(grid, sprintf("(?=(X.{%d}M.{%d}A.{%d}S))", line_length + 1, line_length + 1, line_length + 1))

found

found <- str_count(grid,         sprintf("(?=(M.{1}S.{%d}A.{%d}M.{1}S))", line_length - 1, line_length - 1))
found <- found + str_count(grid, sprintf("(?=(M.{1}M.{%d}A.{%d}S.{1}S))", line_length - 1, line_length - 1))
found <- found + str_count(grid, sprintf("(?=(S.{1}M.{%d}A.{%d}S.{1}M))", line_length - 1, line_length - 1))
found <- found + str_count(grid, sprintf("(?=(S.{1}S.{%d}A.{%d}M.{1}M))", line_length - 1, line_length - 1))

found
