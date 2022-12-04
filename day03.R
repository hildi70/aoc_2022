library(stringr)
library(dplyr)

input <- readLines("input/day03.txt")

first_comp <- substring(input, 1, nchar(input)/2) %>%
  strsplit("")
sec_comp <- substring(input, (nchar(input)/2)+1, nchar(input)) %>%
  strsplit("")
g <- rep(seq_along(first_comp), sapply(first_comp, length))

letters_in_comp1 <- sapply(letters, function(x) unique(g[which(unlist(first_comp) %in% x)]))
letters_in_comp2 <- sapply(letters, function(x) unique(g[which(unlist(sec_comp) %in% x)]))
letters_in_both <- Map(`%in%`, letters_in_comp1, letters_in_comp2) %>%
  sapply(function(x) sum(x))

LETTERS_in_comp1 <- sapply(LETTERS, function(x) unique(g[which(unlist(first_comp) %in% x)]))
LETTERS_in_comp2 <- sapply(LETTERS, function(x) unique(g[which(unlist(sec_comp) %in% x)]))
LETTERS_in_both <- Map(`%in%`, LETTERS_in_comp1, LETTERS_in_comp2) %>%
  sapply(function(x) sum(x))

# check: as many matches as rucksacks?
sum(LETTERS_in_both) + sum(letters_in_both)  

names(letters_in_both) <- c(1:26)
names(LETTERS_in_both) <- c(27:52)

sum_letters <- sum(letters_in_both*as.numeric(names(letters_in_both)))
sum_LETTERS <- sum(LETTERS_in_both*as.numeric(names(LETTERS_in_both)))

sum_letters + sum_LETTERS  # 8088

# part2
input <- readLines("input/day03.txt") %>%
  strsplit("")

input <- sapply(input, function(x) unique(x))

groups <- c(1:100)
letters_per_group <- list()
elf <- 1
letter <- c()

# collect letters per group, their common letter has to appear 3 times
for (group in groups) {
  letters_per_group[group] <- strsplit(paste(sapply(input[elf:(elf+2)], paste, collapse = ""), collapse = ""),"")
  letter[group] <- names(which(table(letters_per_group[[group]]) == 3))
  elf <- elf + 3
}

sum_letters <- c(table(sort(letter[which(letter %in% letters)])), table(sort(letter[which(letter %in% LETTERS)])))

prios <- c(1:52)
letter_points <- c(letters, LETTERS)

sum(prios[which(letter_points %in% names(sum_letters))] * sum_letters) # 2522
