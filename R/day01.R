library(tidyverse)

# read data
raw <- read_delim("data/day01.txt", delim = "\n", col_names = FALSE) %>% 
  rename(report = X1)

prod(intersect(raw$report, 2020 - raw$report))
