library(unglue)
library(tidyverse)

input <- unglue_data(readLines("data/2021/day12.txt"), "{from}-{to}")

# part 1
# how many paths through the caves are there that visit small caves only once?
paths <- input %>% 
  select(from = to, to = from) %>% 
  bind_rows(input) %>% 
  group_by(from) %>% 
  summarize(paths = list(to)) %>% 
  deframe()

traverse <- function(x, visited_small = NULL, p1 = TRUE) {
  if (x == "end") {
    return(list(x))
  }
  
  if (x == "start" && "start" %in% visited_small) {
    return(NULL)
  }
  
  if (str_to_upper(x) != x) {
    visited_small <- c(visited_small, x)
  }
  
  next_possible <- paths[[x]]
  
  if (p1 || any(duplicated(visited_small))) {
    next_possible <- setdiff(next_possible, visited_small)
  }
  
  map(next_possible, traverse, visited_small = visited_small, p1 = p1) %>% 
    do.call(c, .) %>% 
    map(~ c(x, .))
}

length(traverse("start"))

# part 2
# given the new rules, how many paths through the cave system are there?
length(traverse("start", p1 = FALSE))
