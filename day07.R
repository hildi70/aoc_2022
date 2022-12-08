library(stringr)
library(dplyr)

input <- readLines("input/day07.txt")

# renaming duplicate names of directories didn't work due to nested order.
# manual solution:

number_mwl <- c(1, 1, 2, 3, 4, 4, 3, 5, 5, 6, 6, 7, 7, 2, 8, 9, 9, 8, 10, 10, 11, 11, 12, 12)
number_mbtsvblj <-c(1, 1, 2, 2, 3, 3, 4, 5, 5, 4, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13)
number_zcbnwhzd <- c(1, 1, 2, 2, 3, 3, 4, 5, 6, 6, 5, 4, 7, 7, 8, 8)
number_ppmtvcj <- c(1, 1, 2, 3, 3, 2, 4, 4, 5, 5)
number_ccjp <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

counter_mwl <- 0
counter_mbtsvblj <- 0
counter_zcbnwhzd <- 0
counter_ppmtvcj <- 0
counter_ccjp <- 0

for (i in 1:length(input)) {
  if (grepl("dir mwl", input[i]) | grepl("\\$ cd mwl", input[i])) {
    counter_mwl <- counter_mwl + 1
    input[i] <- paste0(input[i], "-", number_mwl[counter_mwl])
  } else if (grepl("dir mbtsvblj", input[i]) | grepl("\\$ cd mbtsvblj", input[i])) {
    counter_mbtsvblj <- counter_mbtsvblj + 1
    input[i] <- paste0(input[i], "-", number_mbtsvblj[counter_mbtsvblj])
  } else if (grepl("dir zcbnwhzd", input[i]) | grepl("\\$ cd zcbnwhzd", input[i])) {
    counter_zcbnwhzd <- counter_zcbnwhzd + 1
    input[i] <- paste0(input[i], "-", number_zcbnwhzd[counter_zcbnwhzd])
  } else if (grepl("dir ppmtvcj", input[i]) | grepl("\\$ cd ppmtvcj", input[i])) {
    counter_ppmtvcj <- counter_ppmtvcj + 1
    input[i] <- paste0(input[i], "-", number_ppmtvcj[counter_ppmtvcj])
  } else if (grepl("dir ccjp", input[i]) | grepl("\\$ cd ccjp", input[i])) {
    counter_ccjp <- counter_ccjp + 1
    input[i] <- paste0(input[i], "-", number_ccjp[counter_ccjp])
  } 
}

# old approach:
# input <- ave(as.character(input), input, FUN = function(x) if (length(x)>1) paste0(x[1], "-", seq_along(x)) else x[1])

size <- as.numeric(str_extract(input, "^[0-9]+"))

type <- c(rep(NA, times = length(input)))
type[grepl("^[0-9]+", input)] <- "file"
type[grepl("\\$", input)]<- "command"
type[grepl("dir ", input)]<- "directory"

# defining parent directory
directory <- c(rep(NA, times = length(input)))
for (i in 2:length(input)) {
  directory[i] <- sub("\\$ cd ", "", input[i-(min(grep("\\$ cd", input[(i-1):1])))])
}

directory[1] <- " /"

df <- as.data.frame(cbind(input, directory, type, size))

# calculating file sizes within a directory (without subdirectories)
directory_sizes <- df %>%
  group_by(directory) %>%
  filter(type == "file") %>%
  summarize("size" = sum(as.numeric(size))) %>%
  ungroup() %>%
  rename(parent = "directory") %>%
  mutate(directory = ".") %>%
  select(parent, directory, size)

# defining subdirectories for each parent
subdirectories <- df %>%
  filter(type == "directory") %>%
  rename(parent = "directory") %>%
  mutate(directory = sub("dir ", "", input)) %>%
  select(parent, directory)


df <- full_join(subdirectories, directory_sizes)

# calculating file sizes per directory
for (i in nrow(df):1) {
  if (is.na(df$size[i])) {
    df$size[i] = sum(df$size[which(df$parent == df$directory[i])])
  }
}

df_grouped <- df %>%
  group_by(parent) %>%
  summarize("size_total" = sum(size))

df_grouped %>%
  filter(size_total <= 100000) %>%
  summarize("solution" = sum(size_total))  # 1513699


## part 2
space_used <- as.numeric(df_grouped[1, 2])
space_to_delete <- 30000000 - (70000000 - space_used)

df_grouped %>%
  filter(size_total >= space_to_delete) %>%
  summarize("solution" = min(size_total))  # 7991939

