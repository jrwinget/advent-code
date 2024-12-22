library(tidyverse)

# read the data
input <- read_fwf(
  "data/2024/day08.txt",
  col_positions = fwf_widths(
    rep(1, nchar(read_lines("data/2024/day08.txt", n_max = 1)))
  )
) |> 
  as.matrix()

# helper functions
is_in_bounds <- function(pos, max_val) {
  all(pos > 0) && all(pos <= max_val)
}

# add antinodes based on spacing and bounds
expand_antinodes <- function(position, spacing, max_val) {
  antinodes <- list(position)
  while (is_in_bounds(position + spacing, max_val)) {
    position <- position + spacing
    antinodes <- append(antinodes, list(position))
  }
  bind_rows(antinodes)
}

# calculate antinodes for specific frequency
calculate_antinodes <- function(freq, input) {
  coords <- which(input == freq, arr.ind = TRUE) |> 
    as_tibble()

  antinodes <- list()

  for (i in seq_len(nrow(coords))) {
    for (j in seq_len(nrow(coords))) {
      if (!all(coords[j, ] == coords[i, ])) {
        spacing <- coords[j, ] - coords[i, ]
        a1 <- coords[i, ] - spacing
        a2 <- coords[j, ] + spacing

        antinodes <- append(antinodes, list(a1, a2))
      }
    }
  }

  bind_rows(antinodes) |> 
    distinct() |> 
    filter(if_all(everything(), ~ .x > 0 & .x <= max(dim(input))))
}

# get all antinodes for the matrix
extract_all_antinodes <- function(input) {
  unique_values <- setdiff(unique(as.vector(input)), ".")

  map_dfr(unique_values, calculate_antinodes, input = input) |> 
    distinct()
}

# how many unique locations contain an antinode?
nrow(extract_all_antinodes(input))
