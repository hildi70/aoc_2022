input <- readLines("input/day10.txt")

circle_type <- vapply(strsplit(input, " ", fixed = TRUE), "[", "", 1)
n_circles <- as.numeric(vapply(strsplit(input, " ", fixed = TRUE), "[", "", 2))
n_circles[is.na(n_circles)] <- 0

# part 1
circle <- 1
v <- 1
targets <- c(20, 60, 100, 140, 180, 220)
v_target <- c(rep(0, length(targets)))

for (i in 1:length(circle_type)) {
  
  if (circle_type[i] == "noop") {
    circle <- circle + 1

  } else {
    circle <- circle +1
    
    if (circle %in% targets) {
      v_target[which(targets == circle)] <- v
    }
    
    circle <- circle + 1
    v <- v + n_circles[i]
  }
  
  if (circle %in% targets) {
    v_target[which(targets == circle)] <- v
  }
}

sum(targets*v_target)  #13720


# part 2
circle <- 0
v <- 1
sprite <- c(v-1, v, v+1)
pixel <- vector(mode = "list", length = 6)
line <- 1

for (i in 1:length(circle_type)) {
  
  if (circle_type[i] == "noop") {
    pixel[[line]][circle+1] <- ifelse ((circle %in% sprite), "#", ".")

    if (circle == 39) {
      line <- line +1
      circle <- 0
    } else {circle <- circle + 1}
    
  } else {
    pixel[[line]][circle+1] <- ifelse ((circle %in% sprite), "#", ".")
    if (circle == 39) {
      line <- line +1
      circle <- 0
    } else {circle <- circle + 1}
    
    pixel[[line]][circle+1] <- ifelse ((circle %in% sprite), "#", ".")
    if (circle == 39) {
      line <- line +1
      circle <- 0
    } else {circle <- circle + 1}
    
    v <- v + n_circles[i]
    sprite <- c(v-1, v, v+1)
  }
}

df <- as.data.frame(do.call(rbind, pixel))
View(df)  # FBURHZCH
