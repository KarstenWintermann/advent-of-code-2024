library(tidyverse)
library(readr)
library(bit64)

filename <- "2024-09.txt"

disk <- strtoi(str_split(read_file(filename), "")[[1]])

disk_len <- length(disk)

left_index <- 1
left_subindex <- disk[[left_index]]

right_index <- disk_len
if ((right_index %% 2) == 0) {
  right_index <- right_index - 1
}
right_subindex <- disk[[right_index]]

block <- 0 
checksum <- as.integer64(0)

blockvalue <- function(len) {
  res <- 0
  if (len != 0) {
    for (i in seq.int(from = block, to = block + len - 1)) {
      res <- res + i
    }
    block <<- block + len
  }
  res
}

while (left_index <= disk_len) {
  if ((left_index %% 2) == 1) {
    # process full block from left
    checksum <- checksum + ((left_index-1)/2) * blockvalue(left_subindex) 
    disk[[left_index]] <- 0
    left_index <- left_index + 1
    if (left_index <= disk_len) {
      left_subindex <- disk[[left_index]]
    } else {
      left_subindex <- 0
    }
  } else {
    # process full blocks from right
    while ((right_subindex <= left_subindex) & (right_index > left_index)) {
      checksum <- checksum + ((right_index-1) / 2) * blockvalue(right_subindex)
      disk[right_index] <- 0
      left_subindex <- left_subindex - right_subindex
      right_index <- right_index - 2
      right_subindex <- disk[[right_index]]
    } 
    # process partial blocks from right
    if (left_subindex > 0) {
      if (disk[[right_index]] > 0) {
        checksum <- checksum + ((right_index-1) / 2) * blockvalue(left_subindex)
      }
      # move right pointer to the left
      right_subindex <- right_subindex - left_subindex
      if (right_subindex < 0) {
        right_subindex = 0
      }
      disk[[right_index]] <- right_subindex
    }
    # move left pointer to the right
    left_index <- left_index + 1
    if (left_index <= disk_len) {
      left_subindex <- disk[[left_index]]
    } else {
      left_subindex <- 0
    }
  }
}

checksum

disk <- strtoi(str_split(read_file(filename), "")[[1]])

disk_len <- length(disk)

right_index <- disk_len
if ((right_index %% 2) == 0) {
  right_index <- right_index - 1
}

block <- 0 
checksum <- as.integer64(0)

pos <- disk

for (i in seq_along(disk)) {
  if (i == 1) {
    pos[[i]] <- 0
  } else {
    pos[[i]] <- pos[[i-1]] + disk[[i-1]]
  }
}

while (right_index > 0) {
  left_index <- 2
  moved <- FALSE
  if (disk[[right_index]] == 0) {
    next
  }
  while (left_index < right_index) {
    if (disk[[left_index]] >= disk[[right_index]]) {
      disk[[left_index]] <- disk[[left_index]] - disk[[right_index]]
      checksum <- checksum + (((pos[[left_index]] * 2) + disk[[right_index]] - 1) / 2) * ((right_index-1)/2) * disk[[right_index]]
      pos[[left_index]] <- pos[[left_index]] + disk[[right_index]]
      moved <- TRUE
      break
    }
    left_index <- left_index + 2
  }
  if (!moved) {
    checksum <- checksum + (((pos[[right_index]] * 2) + disk[[right_index]] - 1) / 2) * ((right_index-1)/2) * disk[[right_index]]
  }
  
  right_index <- right_index - 2 
}

checksum