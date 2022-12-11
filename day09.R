library(stringr)
input <- readLines("input/day09.txt")

direction <- str_extract(input, "[A-Z]")
dir_logical <- ifelse(direction == "D" | direction == "R", 1, -1)

steps <- dir_logical * as.numeric(str_extract(input, "[0-9]+"))
direction <- ifelse(direction == "R" | direction == "L", "h", "v") # h = horizontal, v = vertical


plank <- matrix(0, nrow = 1000, ncol = 1000)

H_vertical <- 500
H_horizontal <- 500
T_vertical <- 500
T_horizontal <- 500

plank[T_horizontal, T_vertical] <- 1


for (step in seq_along(steps)) {
  
  way <- ifelse (direction[step] == "v", 1, 0)
  
  H_vertical_temp <- H_vertical + steps[step] * (!way)
  H_horizontal_temp <- H_horizontal + steps[step] * way
  
  # if H touches T (or is T!) after the move -> no changes
  if ((H_vertical_temp == T_vertical && abs(H_horizontal_temp - T_horizontal) == 1 ) ||
      (H_horizontal_temp == T_horizontal && abs(H_vertical_temp - T_vertical) == 1 ) ||
      # touches diagonal
      (abs(H_vertical_temp - T_vertical) == 1 && abs(H_horizontal_temp - T_horizontal) == 1) ||
      # H is T
      (H_vertical_temp == T_vertical && H_horizontal_temp == T_horizontal)) {
    
    T_vertical <- T_vertical
    T_horizontal <- T_horizontal
    
  } else if (T_horizontal == H_horizontal && direction[step] == "h") {
    
    plank[T_horizontal, T_vertical:(H_vertical_temp + dir_logical[step]*(-1))] <- 1
    T_vertical <- H_vertical_temp + dir_logical[step]*(-1)
    
  } else if (T_vertical == H_vertical && direction[step] == "v") {
    
    plank[T_horizontal:(H_horizontal_temp + dir_logical[step]*(-1)), T_vertical] <- 1
    T_horizontal <- H_horizontal_temp + dir_logical[step]*(-1)
    
  } else if (T_horizontal == H_horizontal && direction[step] == "v") {
    
    T_vertical <- H_vertical_temp
    plank[(T_horizontal + dir_logical[step] * 1):(H_horizontal_temp + dir_logical[step]*(-1)), T_vertical] <- 1
    T_horizontal <- H_horizontal_temp + dir_logical[step]*(-1)
    
  } else if (T_vertical == H_vertical && direction[step] == "h") {
    
    T_horizontal <- H_horizontal_temp
    plank[T_horizontal, (T_vertical + dir_logical[step] * 1):(H_vertical_temp + dir_logical[step]*(-1))] <- 1
    T_vertical <- H_vertical_temp + dir_logical[step]*(-1)
    
  } else if (abs(H_vertical - T_vertical) == 1 && abs(H_horizontal - T_horizontal) == 1) {
    # diagonal starting position, which doesn't result in a "touch"
    # -> direction is important: does H pass by T or does it move away from T?
    
    if (way) # direction = v 
    {
      if ((H_horizontal > T_horizontal && H_horizontal_temp > T_horizontal) ||
          (H_horizontal < T_horizontal && H_horizontal_temp < T_horizontal)) {
        
        T_vertical <- H_vertical
        T_horizontal <- H_horizontal
        
      } else {
        
        T_vertical <- H_vertical
        T_horizontal <- H_horizontal + 2 * dir_logical[step]
        
      }
      
      plank[T_horizontal:(H_horizontal_temp + dir_logical[step]*(-1)), T_vertical] <- 1
      T_horizontal <- H_horizontal_temp + dir_logical[step]*(-1)
      
    } else if (way == 0) {
      
      if ((H_vertical > T_vertical && H_vertical_temp > T_vertical) ||
          (H_vertical < T_vertical && H_vertical_temp < T_vertical)) {
        
        T_vertical <- H_vertical
        T_horizontal <- H_horizontal
        
      } else { # passes by
        
        T_vertical <- H_vertical + 2 * dir_logical[step]
        T_horizontal <- H_horizontal
        
      }
      
      plank[T_horizontal, T_vertical:(H_vertical_temp + dir_logical[step]*(-1))] <- 1
      T_vertical <- H_vertical_temp + dir_logical[step]*(-1)
    }
    
  }
  
  H_vertical <- H_vertical_temp
  H_horizontal <- H_horizontal_temp
  
  
}

sum(plank) # 5878