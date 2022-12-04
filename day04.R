library(stringr)
library(dplyr)

input <- read.delim("input/day04.txt", sep = ",", header = F,
                    col.names = c("assign_1", "assign_2"))

# part 1
input$ass_start_1 <- as.numeric(str_extract(input$assign_1, "[0-9]+"))
input$ass_stop_1 <- as.numeric(str_extract(input$assign_1, "[0-9]+$"))

input$ass_start_2 <- as.numeric(str_extract(input$assign_2, "[0-9]+"))
input$ass_stop_2 <- as.numeric(str_extract(input$assign_2, "[0-9]+$"))

input %>%
  mutate(contains_the_other = case_when(
    (ass_start_1 <= ass_start_2 & ass_stop_1 >= ass_stop_2) ~ 1,
    (ass_start_1 >= ass_start_2 & ass_stop_1 <= ass_stop_2) ~ 1,
    TRUE ~ 0)) %>%
  summarise(unnecessary = sum(contains_the_other))  # 595

# part 2
input %>%
  mutate(overlaps = case_when(
    (ass_start_1 <= ass_start_2 & ass_stop_1 >= ass_start_2) ~ 1,
    (ass_start_2 <= ass_start_1 & ass_stop_2 >= ass_start_1) ~ 1,
    TRUE ~ 0)) %>%
  summarise(unnecessary = sum(overlaps))  # 952
