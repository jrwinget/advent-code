library(tidyverse)

# read the data
input <- read_table("data/2024/day01.txt", col_names = FALSE)

# part 1 solution
# what is the total distance between your lists?
input |> 
  transmute(diff = abs(sort(X1) - sort(X2))) |> 
  summarize(total_distance = sum(diff))
