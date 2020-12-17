library(tidyverse)

# read data
input <- read_csv("data/day03.txt", col_names = FALSE)

# function to count number of trees given slope
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
# starting at the top-left corner of the map and following a slope of 
# right 3 and down 1, how many trees would you encounter?
n_trees(1, 3)

# part 2 solution
# determine the number of trees you would encounter for a series of slopes if
# you start at the top-left corner and traverse the map to the bottom
# then find the product of the number of trees encountered for each slope
tibble(
  x = c(1, 1, 1, 1, 2),
  y = c(1, 3, 5, 7, 1)
  ) %>%
  pmap_dbl(~ n_trees(..1, ..2)) %>%
  reduce(`*`)
