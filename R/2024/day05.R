library(tidyverse)

# read the data
input <- read_lines("data/2024/day05.txt")

split_idx <- which(input == "")[1]

rules <- as_tibble(input[1:(split_idx - 1)]) |>
  separate(value, into = c("before", "after"), sep = "\\|", convert = TRUE)

updates <- as_tibble(input[(split_idx + 1):length(input)]) |>
  mutate(update = str_split(value, ",", simplify = FALSE)) |> 
  select(update)

# part 1 solution
# helper function
validate_update <- function(update, rules) {
  positions <- set_names(seq_along(update), update)
  
  all(map2_lgl(rules$before, rules$after, ~ {
    if (.x %in% update && .y %in% update) {
      positions[as.character(.x)] < positions[as.character(.y)]
    } else TRUE
  }))
}

# what is the sum of the middle page number from the correctly-ordered updates?
updates |>
  mutate(valid = map_lgl(update, ~ validate_update(.x, rules))) |>
  filter(valid) |> 
  mutate(
    middle_page = map_int(
      update,
      ~ as.integer(.x[ceiling(length(.x) / 2)])
    )
  ) |> 
  summarize(total = sum(middle_page))
