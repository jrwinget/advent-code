library(tidyverse)

input <- tibble(x = readLines("data/2021/day08.txt")) %>% 
  extract(
    x,
    c("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "y1", "y2", "y3", "y4"),
    "(\\w+) (\\w+) (\\w+) (\\w+) (\\w+) (\\w+) (\\w+) (\\w+) (\\w+) (\\w+) \\| (\\w+) (\\w+) (\\w+) (\\w+)"
  )

# part 1
# in the output values, how many times do digits 1, 4, 7, or 8 appear?
sum(nchar(unlist(input[11:14])) %in% c(2, 3, 4, 7))

# part 2
# determine all connections and decode the output values for each entry
# what do you get if you add up all of the output values?
num <- tibble(
  x0 = "abcefg", x1 = "cf", x2 = "acdeg", x3 = "acdfg", x4 = "bcdf", 
  x5 = "abdfg", x6 = "abdefg", x7 = "acf", x8 = "abcdefg", x9 = "abcdfg"
)

to_long <- . %>% 
  mutate(line = 1:n()) %>% 
  pivot_longer(-line, names_to = "pos", values_to = "segment") %>% 
  separate_rows(segment, sep = "") %>% 
  filter(segment != "") %>% 
  mutate(input = startsWith(pos, "x")) %>% 
  with_groups(c(line, segment), mutate, n_segment = sum(input)) %>% 
  with_groups(c(line, pos), summarize, key = list(sort(n_segment)))

num_long <- to_long(num) %>% 
  transmute(key, num = 0:9)

input_long <- to_long(input)

left_join(input_long, num_long, by = "key") %>% 
  filter(startsWith(pos, "y")) %>% 
  with_groups(line, summarize, num = sum(num * c(1000, 100, 10, 1))) %>% 
  pull(num) %>% 
  sum()
