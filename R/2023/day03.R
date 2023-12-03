library(tidyverse)

# read data
input <- tibble(x = readLines("data/2023/day03.txt"))

# part 1
# what is the sum of all of the part numbers in the engine schematic?
grid <- input |>
  mutate(row = row_number()) |>
  mutate(value = str_split(x, "")) |>
  select(-x) |>
  unnest(value) |>
  group_by(row) |>
  mutate(col = row_number()) |>
  ungroup() |> 
  mutate(is_digit = str_detect(value, "\\d")) |> 
  group_by(row) |> 
  mutate(id = paste0(
    row, ".", cumsum(is_digit != lag(is_digit, default = FALSE))
  )
  ) |> 
  group_by(id) |> 
  mutate(part = as.numeric(paste0(value, collapse = ""))) |> 
  ungroup()

adj <- bind_rows(
  tibble(
    row_d = c(-1, 1, 0, 0),
    col_d = c(0, 0, -1, 1)
  ),
  tibble(
    row_d = c(-1, -1, 1, 1),
    col_d = c(-1, 1, -1, 1)
  )
)

grid |> 
  filter(!is.na(part)) |> 
  crossing(adj) |>
  mutate(
    row2 = row + row_d,
    col2 = col + col_d
  ) |>
  inner_join(grid, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) |>
  filter(row != row2 | col != col2) |>
  select(-row_d, -col_d) |> 
  filter(value2 != ".", !is_digit2) |> 
  arrange(row, col) |> 
  distinct(id, .keep_all = TRUE) |> 
  summarize(sum(part))

# part 2
# what is the sum of all of the gear ratios in your engine schematic?
grid |> 
  filter(value =="*") |> 
  crossing(adj) |>
  mutate(
    row2 = row + row_d,
    col2 = col + col_d
  ) |>
  inner_join(grid, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) |>
  filter(row != row2 | col != col2) |>
  select(-row_d, -col_d) |> 
  filter(!is.na(part2)) |> 
  distinct(row, col, id2, .keep_all = TRUE) |> 
  group_by(row, col) |> 
  summarize(
    n_adj = n(),
    gear_ratio = prod(part2),
    .groups = "drop"
  ) |> 
  filter(n_adj == 2) |> 
  summarize(sum(gear_ratio))
