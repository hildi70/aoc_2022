library(dplyr)
library(readr)
library(stringr)
library(stringi)

# get the stacks ready
stacks <- readLines("input/day05.txt", n = 8) %>%
  str_extract_all("[A-Z]|    ", simplify = T)

stacks_ordered <- list()

for (i in 1:ncol(stacks)) {
  stacks_ordered[[i]] <- stacks[1:nrow(stacks), i] %>%
    str_replace_all(" ", "") %>%
    stri_remove_empty(na_empty = TRUE)
}

# getting the moving orders
moving_orders <- read_lines("input/day05.txt", skip = 10) %>%
  str_split(" ")

moving_orders <- as.data.frame(do.call(rbind, moving_orders))[c(2, 4, 6)]
names(moving_orders) <- c("n", "from", "to")

counter <- 0

for (order_number in 1:nrow(moving_orders)) {
  
  counter <- as.numeric(moving_orders[order_number, "n"])
  
  while (counter  > 0) {
    if(length(stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]]) > 0) {
      
      stacks_ordered[[as.numeric(moving_orders[order_number, "to"])]] <-
        c(stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]][1],
          stacks_ordered[[as.numeric(moving_orders[order_number, "to"])]])
      
      stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]] <-
        stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]][-1]
    }
    
    counter <- counter - 1
  }
  
}

paste(sapply(stacks_ordered, "[[", 1), collapse = "")  # QMBMJDFTD

# Part 2
stacks <- readLines("input/day05.txt", n = 8) %>%
  str_extract_all("[A-Z]|    ", simplify = T)

stacks_ordered <- list()

for (i in 1:ncol(stacks)) {
  stacks_ordered[[i]] <- stacks[1:nrow(stacks), i] %>%
    str_replace_all(" ", "") %>%
    stri_remove_empty(na_empty = TRUE)
}

moving_orders <- read_lines("input/day05.txt", skip = 10) %>%
  str_split(" ")

moving_orders <- as.data.frame(do.call(rbind, moving_orders))[c(2, 4, 6)]
names(moving_orders) <- c("n", "from", "to")


counter <- 0


for (order_number in 1:nrow(moving_orders)) {
  
  counter <- as.numeric(moving_orders[order_number, "n"])
  
  while (counter  > 0) {
    if(length(stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]]) > 0) {
      
      stacks_ordered[[as.numeric(moving_orders[order_number, "to"])]] <-
        c(stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]][counter],
          stacks_ordered[[as.numeric(moving_orders[order_number, "to"])]])
      
      stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]] <-
        stacks_ordered[[as.numeric(moving_orders[order_number, "from"])]][-counter]
    }
    
    counter <- counter - 1
  }
  
}

paste(sapply(stacks_ordered, "[[", 1), collapse = "")  # NBTVTJNFJ
