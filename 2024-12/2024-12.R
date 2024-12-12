library(tidyverse)
library(readr)

filename <- "2024-12.txt"

# part 1

farm <- 
  str_split(
    str_split(
      read_file(filename), 
      "\r\n")[[1]], 
    "")

visited <- array(FALSE, dim=c(length(farm), length(farm[[1]])))

in_farm <- function(row, col) {
  row > 0 &
  col > 0 & 
  row <= length(farm) &
  col <= length(farm[[1]])
}

visit <- function(row, col, plot) {
  if (!in_farm(row, col)) {
    return(list(0,1))
  }
  if (farm[[row]][[col]] != plot) {
    return(list(0,1))
  } 
  return(plotAreaAndLength(row, col))
}

plotAreaAndLength <- function(row, col) {
  area <- 1
  fence <- 0
  if (visited[[row, col]]) {
    return(list(0,0))
  }
  visited[[row, col]] <<- TRUE
  plot <- farm[[row]][[col]]
  l1 <- visit(row+1, col, plot)
  l2 <- visit(row-1, col, plot)
  l3 <- visit(row, col+1, plot)
  l4 <- visit(row, col-1, plot)
  area <- area + l1[[1]] + l2[[1]] + l3[[1]] + l4[[1]]
  fence <- fence + l1[[2]] + l2[[2]] + l3[[2]] + l4[[2]]

  return(list(area, fence))
}

find_unvisited <- function() {
  for (row in seq_along(farm)) {
    for (col in seq_along(farm[[row]])) {
      if (!visited[[row, col]]) {
        return(list(row,col))
      }
    }
  }
  return(list(0,0))
}

score <- 0
while(TRUE) {
  plot <- find_unvisited()
  if(plot[[1]] != 0) {
    area_and_fence <- plotAreaAndLength(plot[[1]], plot[[2]])
    score <- score + area_and_fence[[1]] * area_and_fence[[2]]
  } else {
    break
  }
} 
  
score

# part 2

visited <- array(FALSE, dim=c(length(farm), length(farm[[1]])))

visit2 <- function(row, col, plot) {
  if (!in_farm(row, col)) {
    return(list(0,0))
  }
  if (farm[[row]][[col]] != plot) {
    return(list(0,0))
  } 
  return(plotAreaAndFences(row, col))
}

inside <- function(row, col, plot) {
  if (!in_farm(row, col)) {
    return(FALSE)
  }
  if (farm[[row]][[col]] != plot) {
    return(FALSE)
  } 
  return(TRUE)
}

plotAreaAndFences <- function(row, col) {
#print(paste("plotAreaAndFences", row, col))  
  area <- 1
  fence <- 0
  if (visited[[row, col]]) {
    return(list(0,0))
  }
  visited[[row, col]] <<- TRUE
  plot <- farm[[row]][[col]]
  l1 <- visit2(row+1, col, plot)
  l2 <- visit2(row-1, col, plot)
  l3 <- visit2(row, col+1, plot)
  l4 <- visit2(row, col-1, plot)

  # find all the corners (inward or outward) because #edges == #corners
  if (inside(row+1, col, plot) & inside(row, col+1, plot) & !inside(row+1, col+1, plot)) {
    fence <- fence + 1
  }
  if (!inside(row+1, col, plot) & !inside(row, col+1, plot)) {
    fence <- fence + 1
  }
  if (inside(row+1, col, plot) & inside(row, col-1, plot) & !inside(row+1, col-1, plot)) {
    fence <- fence + 1
  }
  if (!inside(row+1, col, plot) & !inside(row, col-1, plot)) {
    fence <- fence + 1
  }
  if (inside(row-1, col, plot) & inside(row, col+1, plot) & !inside(row-1, col+1, plot)) {
    fence <- fence + 1
  }
  if (!inside(row-1, col, plot) & !inside(row, col+1, plot)) {
    fence <- fence + 1
  }
  if (inside(row-1, col, plot) & inside(row, col-1, plot) & !inside(row-1, col-1, plot)) {
    fence <- fence + 1
  }
  if (!inside(row-1, col, plot) & !inside(row, col-1, plot)) {
    fence <- fence + 1
  }
  
  area <- area + l1[[1]] + l2[[1]] + l3[[1]] + l4[[1]]
  fence <- fence + l1[[2]] + l2[[2]] + l3[[2]] + l4[[2]]
  
  return(list(area, fence))
}

score <- 0
while(TRUE) {
  plot <- find_unvisited()
  if(plot[[1]] != 0) {
    area_and_fence <- plotAreaAndFences(plot[[1]], plot[[2]])
    score <- score + area_and_fence[[1]] * area_and_fence[[2]]
  } else {
    break
  }
} 

score
