library(dplyr)
input <- readLines("input/day13.txt")

df <- as.data.frame(matrix(c(1,2), ncol = 2))
line <- 1

while (line <= length(input)) {
  df[nrow(df) + 1, 1 ] <- input[line]
  df[nrow(df), 2 ] <- input[line + 1]
  
  line <- line +3
}

df <- df[-1, ]


### Part 1
index <- c(0)

for (pair in 1:nrow(df)) {
  # we compare both packets symbol by symbol, starting at the beginning
  packet1 <- unlist(strsplit(df[pair, 1], ""))
  packet2 <- unlist(strsplit(df[pair, 2], ""))

  symbol <- 1
  while (symbol <= max(c(length(packet1), length(packet2)))) {
    
    # paste "1" "0" to "10"
    if (packet1[symbol] %in% c(0:100) & (packet1[symbol + 1]) %in% c(0:100)) {
      packet1[symbol] <- as.numeric(packet1[symbol])*10 + as.numeric(packet1[symbol + 1])
      packet1 <- packet1[-(symbol+1)]
    }
    
    if (packet2[symbol] %in% c(0:100) & (packet2[symbol + 1]) %in% c(0:100)) {
      packet2[symbol] <- as.numeric(packet2[symbol])*10 + as.numeric(packet2[symbol + 1])
      packet2 <- packet2[-(symbol+1)]
    }
      
      # integer vs. list -> turn integer into list with only this element
    if (packet1[symbol] %in% c(0:100) && packet2[symbol] == "[") {
      packet1 <- c(packet1[1:(symbol-1)], "[", packet1[symbol], "]", packet1[(symbol+1):length(packet1)])
      
    } else if (packet2[symbol] %in% c(0:100) && packet1[symbol] == "[") {
      packet2 <- c(packet2[1:(symbol-1)], "[", packet2[symbol], "]", packet2[(symbol+1):length(packet2)])
      
      # comparing two integers
    } else if (packet1[symbol] %in% c(0:100) && packet2[symbol] %in% c(0:100)) {
      # the smaller number one wins. skip if both are the same.
      if (packet1[symbol] != packet2[symbol]) {
        index[pair] <- (as.numeric(packet1[symbol]) < as.numeric(packet2[symbol])) * pair
        break()
      }
      
      # list that runs out of items first wins -> "]" wins always
    } else if ((packet1[symbol] != "]" && packet2[symbol] == "]") || (packet2[symbol] != "]" && packet1[symbol] == "]")) {
      
      index[pair] <- (packet1[symbol] == "]") * pair
      break()
    }
    
    symbol <- symbol + 1
  }
}

sum(index) ## 6623


### Part 2
df[(nrow(df) + 1):(2*nrow(df)), 1] <- df[,2]
df <- df[,-2]
df <- as.data.frame(df)
df[(nrow(df) + 1), ] <- "[[2]]"
df[(nrow(df) + 1), ] <- "[[6]]"

# we still compare the packets pair-wise. but now we count the numbers of "wins" per comparison
# -> how many packets are smaller?
index <- c(rep(0, times = nrow(df)))
pairs <- combn(1:length(index), 2, simplify = F)

for (pair in 1:length(pairs)) {
  
  packet1 <- unlist(strsplit(df[pairs[[pair]][1], 1], ""))
  packet2 <- unlist(strsplit(df[pairs[[pair]][2], 1], ""))
  
  symbol <- 1
  while (symbol <= max(c(length(packet1), length(packet2)))) {
    
    # paste "1" "0" to "10"
    if (packet1[symbol] %in% c(0:100) & (packet1[symbol + 1]) %in% c(0:100)) {
      packet1[symbol] <- as.numeric(packet1[symbol])*10 + as.numeric(packet1[symbol + 1])
      packet1 <- packet1[-(symbol+1)]
    }
    
    if (packet2[symbol] %in% c(0:100) & (packet2[symbol + 1]) %in% c(0:100)) {
      packet2[symbol] <- as.numeric(packet2[symbol])*10 + as.numeric(packet2[symbol + 1])
      packet2 <- packet2[-(symbol+1)]
    }
    
    # integer vs. list -> turn integer into list with only this element
    if (packet1[symbol] %in% c(0:100) && packet2[symbol] == "[") {
      packet1 <- c(packet1[1:(symbol-1)], "[", packet1[symbol], "]", packet1[(symbol+1):length(packet1)])
      
    } else if (packet2[symbol] %in% c(0:100) && packet1[symbol] == "[") {
      packet2 <- c(packet2[1:(symbol-1)], "[", packet2[symbol], "]", packet2[(symbol+1):length(packet2)])
      
      # comparing two integers
    } else if (packet1[symbol] %in% c(0:100) && packet2[symbol] %in% c(0:100)) {
      # the smaller number one wins. skip if both are the same.
      if ((packet1[symbol] != packet2[symbol]) && (as.numeric(packet1[symbol]) < as.numeric(packet2[symbol]))) {
        index[pairs[[pair]][1]] <- index[pairs[[pair]][1]] + 0
        index[pairs[[pair]][2]] <- index[pairs[[pair]][2]] + 1
        
        break()
        
      } else if ((packet1[symbol] != packet2[symbol]) && (as.numeric(packet1[symbol]) > as.numeric(packet2[symbol]))) {
        index[pairs[[pair]][1]] <- index[pairs[[pair]][1]] + 1
        index[pairs[[pair]][2]] <- index[pairs[[pair]][2]] + 0
      
      break()
    }
      
      
      # list that runs out of items first wins -> "]" wins always
    } else if (packet1[symbol] != "]" && packet2[symbol] == "]") {
      
      index[pairs[[pair]][1]] <- index[pairs[[pair]][1]] + 1
      index[pairs[[pair]][2]] <- index[pairs[[pair]][2]] + 0
      
      break()
      
    } else if (packet2[symbol] != "]" && packet1[symbol] == "]") {
      
      index[pairs[[pair]][1]] <- index[pairs[[pair]][1]] + 0
      index[pairs[[pair]][2]] <- index[pairs[[pair]][2]] + 1

      break()
      
    }
    
    symbol <- symbol + 1
  }
}

index <- index + 1

results <- cbind(df, index) %>%
  arrange(index) %>%
  filter(df == "[[2]]" | df == "[[6]]") %>%
  select(index) %>%
  prod()

results  # 23049
     