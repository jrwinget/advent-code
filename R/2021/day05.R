library(tidyverse)

input <- tibble(x = readLines("data/2021/day05.txt")) %>% 
  extract(
    x,
    c("x1", "y1", "x2", "y2"),
    "(\\d+),(\\d+) -> (\\d+),(\\d+)"
  ) %>% 
  mutate(
    x = map2(x1, x2, seq),
    y = map2(y1, y2, seq)
  )

# part 1
# considering only horizontal and vertical lines, at how many points do at least 
# two lines overlap?
hv_lines <- input %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  unnest(c(x, y))
  
hv_lines %>% 
  count(x, y) %>% 
  summarize(sum(n > 1))

# part 2
# considering all lines (horizontal, vertical, and diagonal), at how many points
# do at least two lines overlap?
input %>% 
  filter(!(x1 == x2 | y1 == y2)) %>% 
  unnest(c(x, y)) %>% 
  bind_rows(hv_lines, .) %>% 
  count(x, y) %>% 
  summarize(sum(n > 1))
