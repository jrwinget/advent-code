library(tidyverse)

# read the data
input <- read_lines("data/2024/day06.txt") |> 
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
guard_pos <- which(input %in% c("^", ">", "v", "<"), arr.ind = TRUE)
guard_row <- ((guard_pos - 1) %% nrow(input)) + 1
guard_col <- ((guard_pos - 1) %/% nrow(input)) + 1
facing <- case_when(
  input[guard_row, guard_col] == "^" ~ "U",
  input[guard_row, guard_col] == ">" ~ "R",
  input[guard_row, guard_col] == "v" ~ "D",
  input[guard_row, guard_col] == "<" ~ "L"
)
input[guard_row, guard_col] <- "."

visited <- tibble(row = guard_row, col = guard_col)
current <- c(guard_row, guard_col)

repeat {
  next_pos <- current + directions[[facing]]
  next_row <- next_pos[1]
  next_col <- next_pos[2]
  
  # out of bounds
  if (
    next_row < 1 || 
      next_row > nrow(input) || 
      next_col < 1 || 
      next_col > ncol(input)
  ) {
    break
  }
  
  # move guard
  if (input[next_row, next_col] == "#") {
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
