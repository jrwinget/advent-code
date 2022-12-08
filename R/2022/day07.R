library(tidyverse)

# read data
input <- tibble(x = readLines("data/2022/day07.txt"))

# change directory function
cd <- function(dir, arg) {
  if(is.na(arg) | arg == "") return(dir)
  if(arg == "..") return(head(dir, -1))
  c(dir, file.path(tail(dir, 1), arg))
}

# track cd lines
dir_sizes <- input |>
  extract(x, c("dir", "size"), "cd (.+)|^([0-9]+)", convert = TRUE)  |>
  mutate(path = Reduce(cd, dir, accumulate = TRUE)) |>
  unnest(path) |>
  count(path, wt = size)

# part 1 solution
# what is the sum of all directories with a total size <= 1000?
dir_sizes |>
  filter(n <= 100000) |>
  summarize(sum(n))

# part 2 solution
# what is the size of the smallest directory that would free up enough space?
dir_sizes |>
  filter(n > (max(dir_sizes$n) + 30000000 - 70000000)) |>
  summarize(min(n))
