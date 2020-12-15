library(tidyverse)

# read data
raw <- read_csv("data/day01.txt", col_names = "report")

# part 1 solution
prod(intersect(raw$report, 2020 - raw$report))

# part 2 solution
prod(intersect(raw$report, 2020 - outer(raw$report, raw$report, "+")))
