library(tidyverse)

# read data
jolts <- as.double(read_lines("data/day10.txt"))

# add 0 and highest jolt, and sort adapters by ascending joltage
jolts <- c(0, sort(jolts), max(jolts) + 3)

# part 1 solution
# count the joltage differences among the outlet, all the adapters, & the device
# what is the # of 1-jolt differences multiplied by the # of 3-jolt differences?
prod(table(diff(jolts)))

# create tibble with joltage differences among outlet, adapters, & device
sequences <- as_tibble(unclass(rle(diff(jolts)))) %>% 
  filter(values == 1) # filter for 1-jolt adapters

# returns total # of arrangements that can connect outlet to device
count_combos <- function(x) {
  sum(choose(x - 1, 0:2))
  }

# add combos column that counts the total # of arrangements for each adapter
sequences <- sequences %>% 
  mutate(combos = map_dbl(lengths, count_combos))

# part 2 solution
# what is the total # of distinct ways you can arrange the adapters to 
# connect the charging outlet to your device?
prod(sequences$combos)
