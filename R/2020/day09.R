library(tidyverse)
library(slider)

# read data
xmas <- read_csv("data/day09.txt", col_names = "value")

# returns true if any 2 numbers sum to x
sum_to <- function(x, v) {
  any(v %in% (x - v))
}

# part 1 solution
# what is the first number that is not the sum of 2 of the 25 numbers before it?
(sub_total <- xmas %>% 
  mutate(last_25 = slide(value, c, .before = 25, .after = -1)) %>% 
  tail(-25) %>% 
  filter(!map2_lgl(value, last_25, sum_to)) %>% 
  pull(value))

# returns a contiguous range that sums to subtotal
sub_sum <- function(x, subtotal) {
  # find combo of forward sum and backward sum that add up to total - subtotal
  foward <- cumsum(x)
  backward <- cumsum(rev(x))
  c <- match(foward, (sum(x) - subtotal - backward))
  
  # start at index, end at length - value
  start <- which(!is.na(c))[1]
  end <- length(x) - c[start]
  x[(start + 1):end]
}

# part 2 solution
# what is the sum of the smallest and largest number in the contiguous range?
range <- sub_sum(xmas$value, sub_total)
min(range) + max(range)
