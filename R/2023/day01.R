library(tidyverse)

# read data
input <- tibble(x = readLines("data/2023/day01.txt"))

# part 1 solution
# what is the sum of all of the calibration values?
input |>
  extract(x, "first", "(\\d)", remove = FALSE) |>
  extract(x, "last", ".*(\\d)", remove = FALSE) |>
  mutate(n = as.numeric(paste0(first, last))) |>
  summarize(sum(n, na.rm = TRUE))

# part 2 solution
# what is the new sum of all of the calibration values?
num <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

input |>
  extract(
    x, 
    "first", 
    "(\\d|one|two|three|four|five|six|seven|eight|nine)", 
    remove = FALSE
  ) |>
  extract(
    x, 
    "last", 
    ".*(\\d|one|two|three|four|five|six|seven|eight|nine)",
  ) |>
  mutate(
    first = coalesce(as.numeric(first), match(first, num)),
    last = coalesce(as.numeric(last), match(last, num)),
    n = as.numeric(paste0(first, last))
  ) |>
  summarize(sum(n, na.rm = TRUE))
  