library(stringr)
library(pracma)

# setup
input <- readLines("input/day11.txt")

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
lcm_vector <- function(x) Reduce(pracma::Lcm, x)

monkeys_lines <- grep("Monkey", input)
monkey <- vector(mode = "list", length = length(monkeys_lines))

for (i in 1:length(monkey)) {
  start_line <- monkeys_lines[i]
  starting_items  <- as.numeric(unlist(str_extract_all(input[start_line+1], "[0-9]+")))
  monkey[[i]]["starting_items"] <- list(starting_items) # numeric!
  monkey[[i]]["counter"] <- 0
  monkey[[i]]["operation"] <- vapply(strsplit(input[start_line+2], "= ", fixed = TRUE), "[", "", 2)
  monkey[[i]]["test_value"] <- as.numeric(unlist(str_extract_all(input[start_line+3], "[0-9]+")))
  # we start counting monkeys with 1, not 0 -> +1
  monkey[[i]]["monkey_true"] <- as.numeric(unlist(str_extract_all(input[start_line+4], "[0-9]+"))) + 1
  monkey[[i]]["monkey_false"] <- as.numeric(unlist(str_extract_all(input[start_line+5], "[0-9]+"))) + 1
}


### Part 1 ###
rounds <- 20
round <- 1
i <- 1 # turn

while (round < rounds + 1) {
  while(length(monkey[[i]][["starting_items"]]) > 0) {
    
    old <- monkey[[i]][["starting_items"]][1]
    operation <- eval(parse(text = monkey[[i]][["operation"]]))
    new <- floor(operation/3)
    
    if (is.wholenumber(new/unlist(monkey[[i]]["test_value"]))) {
      targeted_monkey <- unlist(monkey[[i]]["monkey_true"])
      } else {
      targeted_monkey <- unlist(monkey[[i]]["monkey_false"])
      }
    
    monkey[[i]]["counter"] <- unlist(monkey[[i]]["counter"]) + 1
    monkey[[targeted_monkey]][["starting_items"]][length(monkey[[targeted_monkey]][["starting_items"]])+1] <- new
    monkey[[i]][["starting_items"]] <- monkey[[i]][["starting_items"]][-1]
  }
  
  if (i < length(monkeys_lines)) {i <- i + 1} else {
    round <- round + 1
    i <- 1}
}

counters <- sort(sapply(monkey, "[[", "counter"), decreasing = T)
counters[1] * counters[2]  # 50830

### part 2 ###
test_values <- sapply(monkey, "[[", "test_value")
common_test_value <- lcm_vector(test_values) #9699690

rounds <- 10000
round <- 1
i <- 1 # turn

while (round < rounds + 1) {
  while(length(monkey[[i]][["starting_items"]]) > 0) {
    
    old <- monkey[[i]][["starting_items"]][1]
    operation <- eval(parse(text = monkey[[i]][["operation"]]))
    new <- mod(operation, common_test_value)
    
    if (is.wholenumber(new/unlist(monkey[[i]]["test_value"]))) {
      targeted_monkey <- unlist(monkey[[i]]["monkey_true"])
    } else {
      targeted_monkey <- unlist(monkey[[i]]["monkey_false"])
    }
    
    monkey[[i]]["counter"] <- unlist(monkey[[i]]["counter"]) + 1
    monkey[[targeted_monkey]][["starting_items"]][length(monkey[[targeted_monkey]][["starting_items"]])+1] <- new
    monkey[[i]][["starting_items"]] <- monkey[[i]][["starting_items"]][-1]
  }
  
  if (i < length(monkeys_lines)) {i <- i + 1} else {
    round <- round + 1
    i <- 1}
}

counters <- sort(sapply(monkey, "[[", "counter"), decreasing = T)
counters[1] * counters[2]  # 14399640002
