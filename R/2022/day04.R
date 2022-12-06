library(tidyverse)

# read data
input <- tibble(x = readLines("data/2022/day04.txt")) |>
  extract(x, c("x1","x2","y1","y2"), "(\\d+)-(\\d+),(\\d+)-(\\d+)", convert = T)

# part 1 solution
# in how many assignment pairs does one range fully contain the other?
input |>
  filter(
    (x1 >= y1 & x1 <= y2) & (x2 >= y1 & x2 <= y2) |
      (y1 >= x1 & y1 <= x2) & (y2 >= x1 & y2 <= x2)
  ) |>
  nrow()

# part 2 solution
# in how many assignment pairs do the ranges overlap at all?
input |>
  filter(
    (x1 >= y1 & x1 <= y2) | (x2 >= y1 & x2 <= y2) |
      (y1 >= x1 & y1 <= x2) | (y2 >= x1 & y2 <= x2)
  ) |>
  nrow()
