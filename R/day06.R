library(tidyverse)

# read data
declarations <- tibble(x = read_lines("data/day06.txt"))

answers <- declarations %>% 
  mutate(group = cumsum(x == "")) %>% 
  filter(x != "") %>% 
  add_count(group, name = "group_total") %>% 
  separate_rows(x, sep = "") %>% 
  filter(x != "")

# part 1 solution
# for each group, count the number of questions to which anyone answered yes
# what is the sum of those counts?
answers %>% 
  distinct(group, x) %>% 
  nrow()

# part 2 solution
# for each group, count the number of questions to which everyone answered yes
# what is the sum of those counts?
answers %>% 
  count(group, x, group_total) %>% 
  summarize(solution = sum(n == group_total))
