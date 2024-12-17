library(tidyverse)

# read the data
input <- read_lines("data/2024/day03.txt")

# part 1 solution
# what is the sum of the results of the multiplications?
str_extract_all(input, "mul\\(\\d+,\\d+\\)") |>
  unlist() |>
  str_match("mul\\((\\d+),(\\d+)\\)") |>
  as_tibble() |>
  mutate(
    x = as.numeric(V2),
    y = as.numeric(V3),
    product = x * y
  ) |>
  summarize(total = sum(product, na.rm = TRUE))

# part 2 solution
# what is the sum of the results of just the enabled multiplications?
str_extract_all(input, "do\\(\\)|don't\\(\\)|mul\\(\\d+,\\d+\\)") |>
  unlist() |>
  as_tibble() |>
  mutate(
    state_change = case_when(
      value == "do()" ~ TRUE,
      value == "don't()" ~ FALSE,
      TRUE ~ NA
    ),
    state = accumulate(
      state_change,
      ~ ifelse(is.na(.y), .x, .y),
      .init = TRUE
    )[-1],
    x = str_match(value, "mul\\((\\d+),")[, 2] |> as.numeric(),
    y = str_match(value, ",(\\d+)\\)")[, 2] |> as.numeric(),
    product = if_else(str_detect(value, "^mul\\(") & state, x * y, 0)
  ) |>
  summarize(total = sum(product, na.rm = TRUE))
