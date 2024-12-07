library(tidyverse)
library(readr)
library(tidygraph)
library(igraph)

rows <- read_file("2024-05.txt")
sections <- str_split(rows, "\r\n\r\n")

rules <- read_lines(sections[[1]][[1]])

updates <- read_lines(sections[[1]][[2]])

rules_elems <- str_split(rules, "\\|")

rules_table <- read_delim(I(rules), delim="|", col_names=FALSE)

graph <- make_graph(edges = unlist(rules_elems))

updates <- str_split(updates, ",")

sum_middles <- 0

valid_update <- function(line) {
  res <- TRUE

  rest <- line
  
  while(length(rest) > 0) {
    if (
      rules_table |>
      filter(X2 == head(rest, 1)) |>
      filter(X1 %in% tail(rest, length(rest)-1)) |>
      summarise(n()) |>
      deframe()
      > 0) {
      res <- FALSE  
    }
    
    rest <- tail(rest, length(rest)-1)
  }
  
  res
}

for (i in updates) {
  if (valid_update(i)) {
    sum_middles <- sum_middles + strtoi(i[[(length(i)+1)/2]])
  }
}

sum_middles

sort_iterate <- function(update) {
  rest <- update
  res <- c()
  
  while(length(rest) > 0) {
    before <- 
      rules_table |>
        filter(X2 == head(rest, 1)) |>
        filter(X1 %in% tail(rest, length(rest)-1)) |>
        select(X1)
    
    for (i in seq_along(before)) {
      res <- append(res, as.character(before[[i]]))
    }
    
    #res <- append(res, before)
    res <- append(res, head(rest, 1))
    
    rest <- rest[!rest %in% res]
  }
    
  res
}

sum_middles_fixed <- 0

for (i in updates) {
  if (!valid_update(i)) {
    while (!valid_update(i)) {
      i <- sort_iterate(i)
    }
    
    sum_middles_fixed <- sum_middles_fixed + strtoi(i[[(length(i)+1)/2]])
  }
}

sum_middles_fixed
