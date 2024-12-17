library(tidyverse)

# read the data
grid <- read_lines("data/2024/day06.txt") |> 
  strsplit("") |>
  do.call(what = rbind) |> 
  as.matrix()

# helper functions
directions <- list("U" = c(-1, 0), "R" = c(0, 1), "D" = c(1, 0), "L" = c(0, -1))
turn_right <- function(direction) {
  switch(
    direction, 
    "U" = "R", 
    "R" = "D", 
    "D" = "L", 
    "L" = "U"
  )
}

# guard's starting position and direction
guard_pos <- which(grid %in% c("^", ">", "v", "<"), arr.ind = TRUE)
guard_row <- ((guard_pos - 1) %% nrow(grid)) + 1
guard_col <- ((guard_pos - 1) %/% nrow(grid)) + 1
facing <- case_when(
  grid[guard_row, guard_col] == "^" ~ "U",
  grid[guard_row, guard_col] == ">" ~ "R",
  grid[guard_row, guard_col] == "v" ~ "D",
  grid[guard_row, guard_col] == "<" ~ "L"
)
grid[guard_row, guard_col] <- "."

visited <- tibble(row = guard_row, col = guard_col)
current <- c(guard_row, guard_col)

repeat {
  next_pos <- current + directions[[facing]]
  next_row <- next_pos[1]
  next_col <- next_pos[2]
  
  # out of bounds
  if (
    next_row < 1 || 
      next_row > nrow(grid) || 
      next_col < 1 || 
      next_col > ncol(grid)
  ) {
    break
  }
  
  # move guard
  if (grid[next_row, next_col] == "#") {
    facing <- turn_right(facing)
  } else {
    current <- c(next_row, next_col)
    visited <- add_row(visited, row = current[1], col = current[2])
  }
}

# how many distinct positions will the guard visit?
visited |> 
  distinct() |> 
  summarize(total = n())
