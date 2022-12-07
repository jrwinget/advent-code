library(tidyverse)

# read data
input <- readLines("data/2022/day05.txt")
cargo <- read_fwf("data/2022/day05.txt", n_max = 8)
steps <- input[11:length(input)]

# crate mover function
move_crt <- function(step, stacks, crate_mover = 9000) {
  vals <- as.numeric(str_extract_all(step, "\\d+")[[1]])
  names(vals) <- c("move", "from", "to")
  
  if(crate_mover == 9000) {
    move <- rev(tail(stacks[[vals["from"]]], vals["move"]))
  } else if (crate_mover == 9001) {
    move <- tail(stacks[[vals["from"]]], vals["move"])
  }
  
  stacks[[vals["to"]]] <- c(stacks[[vals["to"]]], move)
  
  stacks[[vals["from"]]] <- head(
    stacks[[vals["from"]]], -as.numeric(vals["move"])
  )
  
  stacks
}
  
# part 1 solution
# after the rearrangement, what crate ends up on top of each stack?
stacks <- as.list(cargo) |>
  lapply(function(x) rev(x[!is.na(x)]))

for(i in seq_along(steps)) {
  stacks <- move_crt(steps[i], stacks)
}

stacks |>
  map_chr(last) |>
  str_extract_all("[A-Z]") |>
  paste(collapse = "")

# part 2
# after the actual rearrangement, what crate ends up on top of each stack?
stacks <- as.list(cargo) |>
  lapply(function(x) rev(x[!is.na(x)]))

for(i in seq_along(steps)) {
  stacks <- move_crt(steps[i], stacks, 9001)
}

stacks |>
  map_chr(last) |>
  str_extract_all("[A-Z]") |>
  paste(collapse = "")
