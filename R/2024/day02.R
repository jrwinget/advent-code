library(tidyverse)

# read the data
input <- read_lines("data/2024/day02.txt")

# part 1 solution
# helper function
is_safe <- function(vals) {
  diffs <- diff(vals)
  monotonic <- all(diffs > 0) || all(diffs < 0)
  within_range <- all(abs(diffs) >= 1 & abs(diffs) <= 3)
  monotonic && within_range
}

# how many reports are safe?
input |>
  map(~ str_split(.x, "\\s+")[[1]] |> as.numeric()) |>
  map_lgl(is_safe) |>
  sum()

# part 2 solution
# helper function
is_safe_dampener <- function(vals) {
  if (is_safe(vals)) return(TRUE)
  if (length(vals) <= 2) return(FALSE)
  if (any(map_lgl(seq_along(vals), ~ is_safe(vals[-.x])))) return(TRUE)
  FALSE
}

# now, how many reports are safe?
input |>
  map(\(x) str_split(x, "\\s+")[[1]] |> as.numeric()) |>
  map_lgl(is_safe_dampener) |>
  sum()
