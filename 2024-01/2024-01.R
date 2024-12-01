library(tidyverse)
library(readr)

rows <- read_table(read_file("2024-01test.txt"), col_names = FALSE, col_types = "ii")
X1 <- arrange(select(rows, X1), X1)
X2 <- arrange(select(rows, X2), X2)

bind_cols(X1, X2) |> 
  mutate (dist = abs(X1 - X2)) |> 
  summarise(total_dist = sum(dist))

sim <- function(x) {
  output <- vector("integer", 1)
  for (i in seq_along(x)) {
    output[[i]] = length(which(X2 == x[[i]])) * x[[i]]
  }
  output
}

rows |>
  mutate(similarity = sim(X1)) |>
  summarise(total_similarity = sum(similarity))

