library(tidyverse)

input <- read.delim("data/day03.txt", header = FALSE, stringsAsFactors = FALSE)

n_trees <- function(
  by_x, # height of slope
  by_y # width of slope
){
  # x ranges from 1 to number of rows in input
  x <- seq(1, nrow(input), by = by_x)
  
  # y ranges from 1 to length of x at 3 units per step
  y <- seq(1, length.out = length(x), by = by_y)
  
  list(
    x = x,
    y = y
  ) %>% 
    pmap_dbl( ~ {
      # current row
      row <- input[..1, ]
      # if row isn't wide enough, expand it
      while(nchar(row) < ..2){
        row <- paste0(row, row)
      }
      # split it
      row <- str_split(row, "")[[1]]
      # check if y is a tree or not
      if (row[..2] == ".") return(0)
      if (row[..2] == "#") return(1)
    }) %>% 
    sum()
}

# part 1 solution
n_trees(1, 3)

# part 2 solution
tibble(
  x = c(1, 1, 1, 1, 2),
  y = c(1, 3, 5, 7, 1)
  ) %>%
  pmap_dbl(~ n_trees(..1, ..2)) %>%
  reduce(`*`)
