library(tidyverse)

# read data
tickets <- read_lines("data/day05.txt")

# create matrix
mtrx <- tickets %>% 
  str_split("") %>% 
  do.call(rbind, .)

# use a little matrix algebra
row <- (mtrx[, 1:7] == "B") %*% 2 ^ (6:0)
col <- (mtrx[, 8:10] == "R") %*% 2 ^ (2:0)

seat_id <- 8 * row + col

# part 1 solution
# what is the highest seat id on a boarding pass?
max(seat_id)

# part 2 solution
# find the missing seat number
setdiff(seq(min(seat_id), max(seat_id)), seat_id)
