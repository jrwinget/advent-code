library(tidyverse)

# read data
input <- read.fwf("data/2021/day03.txt", widths = rep(1, 12))

# part 1
# calculate the gamma and epsilon rates, then multiply them together
# what is the power consumption of the submarine represented in decimal?
most_common <- function(x) {
  ceiling(median(x))
}

to_decimal <- function(x) {
  strtoi(paste0(x, collapse = ""), base = 2)
}

gamma <- apply(input, 2, most_common)
epsilon <- 1 - gamma

to_decimal(gamma) * to_decimal(epsilon)

# part 2
# calculate the oxygen generator & CO2 scrubber rating, then multiply together
# what is the life support rating of the submarine?
oxygen <- co2 <- input

i <- 1
while(nrow(oxygen) > 1) {
  x <- most_common(oxygen[, i])
  oxygen <- oxygen[oxygen[, i] == x, ]
  i <- i + 1
} 

i <- 1
while(nrow(co2) > 1) {
  x <- (1 - most_common(co2[, i]))
  co2 <- co2[co2[, i] == x, ]
  i <- i + 1
} 

to_decimal(oxygen) * to_decimal(co2)
