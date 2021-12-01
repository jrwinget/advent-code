library(tidyverse)

# starting numbers
starting_num <- c(20, 9, 11, 0, 1, 2)

# length of starting numbers
i <- length(starting_num)

# play memory game
while (i < 2021) {
  last <- starting_num[i]
  starting_num <- c(
    starting_num,
    if_else(last %in% starting_num[-i], i - max(which(starting_num[-i] == last)), 0))
    i <- i + 1
}

# part 1 solution
# given starting numbers, what will be the 2020th number spoken?
last

# starting numbers
starting_num <- c(20, 9, 11, 0, 1, 2)

# store 30,000,000 places
mem <- numeric(3 * 10^7)

# generate sequence
for (i in seq_along(starting_num)) {
  mem[starting_num[i] + 1] <- i
}

cur_val <- 0 # current value
i <- length(starting_num) + 1
max_i <- 3 * 10^7 # maximum value: 30,000,000

# part 2 solution
# given starting numbers, what will be the 30000000th number spoken?
while (i <= max_i) {
  if (i == max_i) print(cur_val)
  if (!mem[cur_val + 1]) {
    mem[cur_val + 1] <- i
    cur_val <- 0
  } else {
    next_cur_val <- i - mem[cur_val + 1]
    mem[cur_val + 1] <- i
    cur_val <- next_cur_val
  }
  i <- i + 1
}
