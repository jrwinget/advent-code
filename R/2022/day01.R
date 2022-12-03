library(tidyverse)

# read data
input <- tibble(x = as.numeric(readLines("data/2022/day01.txt")))

# part 1 solution
# find the elf carrying the most calories and total that elf's calories
pt1 <- input |> 
  mutate(elf = cumsum(is.na(x))) |> 
  count(elf, wt = x) |> 
  arrange(desc(n))

# part 2 solution
# find the top 3 elves carrying the most calories and total those calories
pt1 |> 
  head(3) |> 
  summarize(sum(n))
