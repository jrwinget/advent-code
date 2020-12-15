library(tidyverse)

# read data
raw <- read_delim("data/day01.txt", delim = "\n", col_names = FALSE) %>% 
  rename(report = X1)

# part 1 solution
prod(intersect(raw$report, 2020 - raw$report))

# part 2 solution
prod(intersect(raw$report, 2020 - outer(raw$report, raw$report, "+")))
