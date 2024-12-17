library(tidyverse)

# read the data
input <- read_lines("data/2024/day04.txt") |>
  map(~ str_split(.x, "")[[1]]) |>
  do.call(what = rbind)

# part 1 solution
# helper function
extract_sequences <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  # horizontal & vertical sequences
  sequences <- list(
    horizontal = map(1:rows, ~ paste(grid[.x, ], collapse = "")),
    horizontal_rev = map(1:rows, ~ paste(rev(grid[.x, ]), collapse = "")),
    vertical = map(1:cols, ~ paste(grid[, .x], collapse = "")),
    vertical_rev = map(1:cols, ~ paste(rev(grid[, .x]), collapse = ""))
  )
  
  # diagonal sequences
  diagonals <- list(
    diag1 = map(
      -(rows - 1):(cols - 1), 
      ~ paste(grid[row(grid) - col(grid) == .x], collapse = "")
    ),
    diag1_rev = map(
      -(rows - 1):(cols - 1), 
      ~ paste(rev(grid[row(grid) - col(grid) == .x]), collapse = "")
    ),
    diag2 = map(
      2:(rows + cols), 
      ~ paste(grid[row(grid) + col(grid) == .x], collapse = "")
    ),
    diag2_rev = map(
      2:(rows + cols),
      ~ paste(rev(grid[row(grid) + col(grid) == .x]), collapse = "")
    )
  )
  
  c(sequences, diagonals) |>
    unlist() |> 
    discard(~ .x == "")
}

# how many times does an X-MAS appear?
input |>
  extract_sequences() |>
  str_count("XMAS") |>
  sum()
