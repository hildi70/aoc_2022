### Part 1
input <- readLines("input/day12.txt")
nrows <- length(input)
input <- as.data.frame(matrix(unlist(strsplit(input, "")), nrow = nrows, byrow = T))
input <- lapply(input, function(x) match(x, c(letters, "E", "S")))
input <- do.call(rbind, input)
input <- rbind(c(rep(0, times = ncol(input))), input, c(rep(0, times = ncol(input))))
input <- cbind(c(rep(0, times = nrow(input))), input, c(rep(0, times = nrow(input))))

up <- c(0, 1, 0)
middle <- c(1, 0, 1)
down <- c(0, 1, 0)
neighbours <- rbind(up, middle, down)

start_coordinates <- which(input == 28, arr.ind = T)  # height = z
# position_coordinates <- which(input == "S", arr.ind = T)  # height = a
input[start_coordinates] <- 0


final_steps <- 0
map <- list(start = input)
all_steps <- c(0)
all_coordinates <- as.data.frame(start_coordinates)
possibilities <- 0

while (length(map) > 0) {
  
  if((length(final_steps[final_steps != 0]) > 0) && (all_steps[1] > min(final_steps[final_steps != 0]))) {
    map <- map[-1]
    all_steps <- all_steps[-1]
    all_coordinates <- all_coordinates[-1, ]
  }
  
  if(length(map) == 0) {break}
  
  input <- map[[1]]
  coordinates <- all_coordinates[1, ]
  steps <- all_steps[1]
  
  if (input[coordinates$row, coordinates$col] == 27) {
    final_steps[length(final_steps) + 1] <- steps
    final_steps <- min(final_steps[final_steps > 0])
    
  } else {
    
    needed_step <- as.numeric(input[coordinates$row, coordinates$col]) + 1
    # rand!
    area <- input[(coordinates$row - 1):(coordinates$row + 1), (coordinates$col - 1):(coordinates$col + 1)]
    area[which(neighbours == 0, arr.ind = T)] <- 0
    
    new_positions <- which(area <= needed_step & area > 0, arr.ind = T)
    
    if (nrow(new_positions) > 0) {
      
      coordinates_fixed <- coordinates
      input[coordinates$row, coordinates$col] <- 0
      steps <- steps + 1
      
      while (nrow(new_positions) > 0) {
        new_position <- new_positions[1, ]
        possibilities <- possibilities + 1
        coordinates <- coordinates_fixed + new_position - 2
        
        # if coordinates kommen schon vor -> geringere steps behalten, sonst:
        index <- which((all_coordinates$row == coordinates$row) & (all_coordinates$col == coordinates$col))
        if (length(index) > 0) {
          if (all_steps[index] > steps)
          {
            map <- map[-index]
            all_steps <- all_steps[-index]
            all_coordinates <- all_coordinates[-index, ]
            
            map[[length(map) + 1]] <- input
            all_steps[length(all_steps) + 1] <- steps
            all_coordinates[(nrow(all_coordinates) + 1), ] <- coordinates
          }
          
        } else {
          
          map[[length(map) + 1]] <- input
          all_steps[length(all_steps) + 1] <- steps
          all_coordinates[(nrow(all_coordinates) + 1), ] <- coordinates
          
        }
        
        new_positions <- rbind(new_positions[-1, ])
      }
    }
  }
  
  map <- map[-1]
  all_steps <- all_steps[-1]
  all_coordinates <- all_coordinates[-1, ]
  
  print(length(all_steps))

}

final_steps  # 456

### Part 2
rm()
input <- readLines("input/day12.txt")
nrows <- length(input)
input <- as.data.frame(matrix(unlist(strsplit(input, "")), nrow = nrows, byrow = T))
input <- lapply(input, function(x) match(x, c(letters, "E", "S")))
input <- do.call(rbind, input)
input <- rbind(c(rep(0, times = ncol(input))), input, c(rep(0, times = ncol(input))))
input <- cbind(c(rep(0, times = nrow(input))), input, c(rep(0, times = nrow(input))))

up <- c(0, 1, 0)
middle <- c(1, 0, 1)
down <- c(0, 1, 0)
neighbours <- rbind(up, middle, down)

start_coordinates <- which(input == 27, arr.ind = T)  # height = z
# position_coordinates <- which(input == "S", arr.ind = T)  # height = a
input[start_coordinates] <- 27


final_steps <- 0
map <- list(start = input)
all_steps <- c(0)
all_coordinates <- as.data.frame(start_coordinates)
possibilities <- 0

while (length(map) > 0) {
  
  if((length(final_steps[final_steps != 0]) > 0) && (all_steps[1] > min(final_steps[final_steps != 0]))) {
    map <- map[-1]
    all_steps <- all_steps[-1]
    all_coordinates <- all_coordinates[-1, ]
  }
  
  if(length(map) == 0) {break}
  
  input <- map[[1]]
  coordinates <- all_coordinates[1, ]
  steps <- all_steps[1]
  
  # goal: any a
  if (input[coordinates$row, coordinates$col] == 1) {
    final_steps[length(final_steps) + 1] <- steps
    final_steps <- min(final_steps[final_steps > 0])
    
  } else {
    
    needed_step <- as.numeric(input[coordinates$row, coordinates$col]) - 1
    # rand!
    area <- input[(coordinates$row - 1):(coordinates$row + 1), (coordinates$col - 1):(coordinates$col + 1)]
    area[which(neighbours == 0, arr.ind = T)] <- 0
    
    new_positions <- which(area >= needed_step & area > 0, arr.ind = T)
    
    if (nrow(new_positions) > 0) {
      
      coordinates_fixed <- coordinates
      input[coordinates$row, coordinates$col] <- 0
      steps <- steps + 1
      
      while (nrow(new_positions) > 0) {
        new_position <- new_positions[1, ]
        possibilities <- possibilities + 1
        coordinates <- coordinates_fixed + new_position - 2
        
        # if coordinates kommen schon vor -> geringere steps behalten, sonst:
        index <- which((all_coordinates$row == coordinates$row) & (all_coordinates$col == coordinates$col))
        if (length(index) > 0) {
          if (all_steps[index] > steps)
          {
            map <- map[-index]
            all_steps <- all_steps[-index]
            all_coordinates <- all_coordinates[-index, ]
            
            map[[length(map) + 1]] <- input
            all_steps[length(all_steps) + 1] <- steps
            all_coordinates[(nrow(all_coordinates) + 1), ] <- coordinates
          }
          
        } else {
          
          map[[length(map) + 1]] <- input
          all_steps[length(all_steps) + 1] <- steps
          all_coordinates[(nrow(all_coordinates) + 1), ] <- coordinates
          
        }
        
        new_positions <- rbind(new_positions[-1, ])
      }
    }
  }
  
  map <- map[-1]
  all_steps <- all_steps[-1]
  all_coordinates <- all_coordinates[-1, ]
  
  print(length(all_steps))
  
}

final_steps # 454
