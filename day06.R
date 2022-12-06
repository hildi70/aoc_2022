input <- scan("input/day06.txt", what = character())
input <- unlist(strsplit(input, ""))


# Part 1
# Find first 4 distinct letters
i <- 1
while (length(unique(input[i:(i+3)])) != 4) {
  i <- i + 1
}

print(paste("Position Marker: ", i+3)) # Position Marker:  1042


# Part 2
# Find first 14 distinct letters
i <- 1
while (length(unique(input[i:(i+13)])) != 14) {
  i <- i + 1
}

print(paste("Position Message: ", i+13))  # Position Message:  2980