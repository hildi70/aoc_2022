library(dplyr)

# part 1
input <- read.table("input/day02.txt")

letter <- c("A", "B", "C", "X", "Y", "Z")
number <- rep(c(1:3), 2)

# replace letters with numbers
input[,1:2] <- number[match(c(input$V1, input$V2), letter)]

input$score <- input$V1 - input$V2

# score of 0 = draw
# score of -1 or 2 = win
# score of 1 or -2 = loss

score <- c(0, -1, 2, 1, -2)  
points <- c(3, 6, 6, 0, 0)

input$points <- points[match(input$score, score)]
sum(input$V2) + sum(input$points)  # 15632

## part 2
input <- read.table("input/day02.txt")

letter <- c("A", "B", "C", "X", "Y", "Z")
number <- c(1, 2, 3, 0, 3, 6)

input[,1:2] <- number[match(c(input$V1, input$V2), letter)]

# depending on the opponents shape and the outcome of the game, my shape varies:
# when opponent draws a 1 and the game ends in a loss, I had to draw a 3, ...
points <- c(0, 3, 6)
shape_1 <- c(3, 1, 2)
shape_2 <- c(1, 2, 3)
shape_3 <- c(2, 3, 1)

input <- input %>%
  mutate(shape = case_when(V1 == 1 ~ shape_1[match(V2, points)],
                           V1 == 2 ~ shape_2[match(V2, points)],
                           V1 == 3 ~ shape_3[match(V2, points)])
  )

sum(input$V2) + sum(input$shape) # 14416
