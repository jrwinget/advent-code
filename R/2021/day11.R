library(tidyverse)

input <- tibble(loc = readLines("data/2021/day11.txt")) %>% 
  mutate(
    row = row_number(),
    value = str_split(loc, "")
  ) %>% 
  unnest(value) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(row) %>% 
  mutate(col = row_number()) %>% 
  ungroup()

adj <- tibble(
  x = c(-1, 1, 0, 0),
  y = c(0, 0, -1, 1)
) %>% 
  bind_rows(
    tibble(
      x = c(-1, -1, 1, 1),
      y = c(-1, 1, -1, 1)
    )
  )

neighbors <- function(df, df2) {
  df %>% 
    crossing(adj) %>% 
    mutate(
      row2 = row + x,
      col2 = col + y
    ) %>% 
    inner_join(df2, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) %>% 
    filter(row != row2 | col != col2) %>% 
    select(-x, -y)
}

# part 1
# simulate 100 steps of the the dumbo octopuses energy/flashes
# how many total flashes are there after 100 steps?
p1 <- 0
for (i in 1:1000) { # use 1:100 for part 1
  input <- input %>% 
    mutate(
      value = value + 1,
      flash = value > 9,
      has_flashed = flash
    )
  
  while (any(input$flash)) {
    p1 <- p1 + sum(input$flash) # part 1
    input <- input %>% 
      neighbors(input) %>% 
      group_by(row, col, value, flash, has_flashed) %>% 
      summarize(value = first(value) + sum(flash2), .groups = "drop") %>% 
      mutate(
        flash = value > 9 & !has_flashed,
        has_flashed = flash | has_flashed
      )
    # part 2
    # what is the first step during which all octopuses flash? 
    if (all(input$has_flashed | input$flash)) {
      stop(paste0("All octopuses have flashed as of step ", i))
    }
  }
  input <- mutate(input, value = ifelse(has_flashed, 0, value))
}
