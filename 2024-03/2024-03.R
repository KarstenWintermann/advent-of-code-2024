library(tidyverse)
library(readr)

input_filename <- "2024-03.txt"

rows <- as_tibble_col(read_lines(input_filename), column_name="X1")

eval_line <- function(x) {
  res = vector("integer", 1)
  for (i in seq_along(x)) {
    res[[i]] = 
        separate_wider_regex(
          tibble(str_extract_all(x[[i]], "mul\\([0-9]+,[0-9]+\\)")[[1]]),
          patterns = c("mul\\(", op1 = "[0-9]+", ",", op2 = "[0-9]+", "\\)"),
          cols = everything()) |>
      mutate(product = strtoi(op1) * strtoi(op2)) |>
      summarise(line_sum = sum(product)) |>
      deframe()
  }
  res
}

rows |>
  mutate(line_sum = eval_line(X1)) |>
  summarise(all_products = sum(line_sum))

rows <- tibble(X1 =
  str_replace_all(
    paste(
      read_lines(input_filename), collapse=""), 
    "don\\'t\\(\\)((?!do\\(\\)).)*(do\\(\\)|$)", "")
  )

rows |>
  mutate(line_sum = eval_line(X1)) |>
  summarise(all_products = sum(line_sum))
