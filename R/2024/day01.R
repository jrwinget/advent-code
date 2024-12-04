library(tidyverse)

# read the data
input <- read_table("data/2024/day01.txt", col_names = FALSE)

# part 1 solution
# what is the total distance between your lists?
input |> 
  transmute(diff = abs(sort(X1) - sort(X2))) |> 
  summarize(total_distance = sum(diff))

# part 2 solution
# what is their similarity score?
right_counts <- count(input, X2, name = "right_count")

input |>
  left_join(right_counts, by = c("X1" = "X2")) |>
  mutate(
    right_count = coalesce(right_count, 0),
    similarity = X1 * right_count
  ) |>
  summarize(similarity_score = sum(similarity))
