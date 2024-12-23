library(tidyverse)
library(readr)
library(tidygraph)
library(igraph)

network <- tbl_graph(
  edges=read_delim(
    read_file("2024-23.txt"), 
    delim="-", 
    col_names = FALSE), 
  directed=FALSE)

c <- cliques(network, min=3, max=3)

sum <- 0
for (i in seq_along(c)) {
  f <- str_detect(c[[i]]$name, "t.")
  if (f[[1]] | f[[2]] | f[[3]]) {
    sum <- sum + 1
  }
}
sum

lc <- largest_cliques(network)
sort(lc[[1]]$name)