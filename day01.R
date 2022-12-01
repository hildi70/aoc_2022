input <- scan("input/day01.txt", blank.lines.skip = F)
input <- replace(input, is.na(input), 0)

cumsum_grouped <- ave(input, cumsum(input == 0), FUN = cumsum)
max(cumsum_grouped)  # 67633
sum(sort(cumsum_grouped, decreasing = T)[1:3])  # 199628
