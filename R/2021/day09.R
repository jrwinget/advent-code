library(tidyverse)

input <- tibble(loc = readLines("data/2021/day09.txt")) %>% 
  mutate(
    row = row_number(),
    value = str_split(loc, "")
  ) %>% 
  unnest(value) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(row) %>% 
  mutate(col = row_number()) %>% 
  ungroup()

# part 1
# find all low points on the heightmap, & sum the risk levels of all low points
adjacent <- tibble(
  x = c(-1, 1, 0, 0),
  y = c(0, 0, -1, 1)
)

neighbors4 <- function(df, df2) {
  df %>% 
    crossing(adjacent) %>% 
    mutate(
      row2 = row + x,
      col2 = col + y
    ) %>% 
    inner_join(df2, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) %>% 
    filter(row != row2 | col != col2)
}

low_points <- input %>% 
  neighbors4(input) %>% 
  group_by(row, col) %>% 
  summarize(low = all(value < value2)) %>% 
  filter(low) %>% 
  inner_join(input, by = c("row", "col")) %>% 
  ungroup()

low_points %>% 
  summarize(sum(1 + value))

# part 2
# find the three largest basins and multiply their sizes together
basins <- caves <- low_points %>% 
  mutate(basin = paste(row, col)) %>% 
  select(basin, row, col, value)

while (nrow(caves) > 0) {
  caves <- caves %>% 
    neighbors4(input) %>% 
    filter(value2 > value, value2 < 9) %>% 
    select(basin, row = row2, col = col2, value = value2) %>% 
    distinct(basin, row, col, .keep_all = TRUE) %>% 
    anti_join(basins, by = c("row", "col"))
  
  basins <- bind_rows(basins, caves)
}

basins %>% 
  count(basin, sort = TRUE) %>% 
  head(3) %>% 
  summarize(prod(n))
