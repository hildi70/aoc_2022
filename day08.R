library(dplyr)
input <- readLines("input/day08.txt")
nrows <- length(input)
input <- matrix(as.numeric(unlist(strsplit(input, ""))), nrow = nrows, byrow = T)
input_logical <- matrix(rep(0, nrows*nrows), nrow = nrows)

for (column in 1:ncol(input)) {
  for (row in 1:nrow(input)) {
    if (column == 1 | row == 1) {
      input_logical[row, column] <- 1
    } else if (column == ncol(input) | row == nrow(input)) {
      input_logical[row, column] <- 1
    } else if(input[row, column] > max(input[c(1:(row-1)), column])) {
      input_logical[row, column] <- 1
    } else if (input[row, column] > max(input[row, c(1:(column-1))])) {
      input_logical[row, column] <- 1
    } else if(input[row, column] > max(input[c((row+1):nrow(input)), column])) {
      input_logical[row, column] <- 1
    } else if (input[row, column] > max(input[row, c((column+1):ncol(input))])) {
      input_logical[row, column] <- 1
    }
  }
}

sum(input_logical) #  1803


# viewing distance
viewing_distance <-  matrix(rep(0, nrows*nrows), nrow = nrows)
up <- 0
down <- 0
left <- 0
right <- 0

for (column in 1:ncol(input)) {
  for (row in 1:nrow(input)) {
    # up
    if (row == 1) {
      up <- 0
    } else if(input[row, column] > max(input[c(1:(row-1)), column])) {
      up <- row - 1
    } else {
      up <- row - max(which(input[c(1:(row-1)), column] >= input[row, column]))
    }
      
    # down
    if (row == nrow(input)) {
      down <- 0
    } else if(input[row, column] > max(input[c((row+1):nrow(input)), column])) {
      down <- nrow(input) - row
    } else {
      down <- min(which(input[c((row+1):nrow(input)), column] >= input[row, column]))
    }
    
    # left
    if (column == 1) {
      left <- 0
    } else if(input[row, column] > max(input[row, c(1:(column-1))])) {
      left <- column - 1
    } else {
      left <- column - max(which(input[row, c(1:(column-1))] >= input[row, column]))
    }
    
    # right
    if (column == ncol(input)) {
      right <- 0
    } else if(input[row, column] > max(input[row, c((column+1):ncol(input))])) {
      right <- ncol(input) - column
    } else {
      right <- min(which(input[row, c((column+1):ncol(input))] >= input[row, column]))
    }
    
    viewing_distance[row, column] <- up*left*right*down
  }
}

max(viewing_distance)  # 268912