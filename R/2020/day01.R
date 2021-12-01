library(tidyverse)

# read data
raw <- read_csv("data/day01.txt", col_names = "report")

# part 1 solution
# find the two entries that sum to 2020 and multiply them together
prod(intersect(raw$report, 2020 - raw$report))

# part 2 solution
# what is the product of the three entries that sum to 2020?
prod(intersect(raw$report, 2020 - outer(raw$report, raw$report, "+")))
