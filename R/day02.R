library(tidyverse)

# read data
passwords <- read_csv("data/day02.txt", col_names = "policy") %>% 
  extract(policy, c("min", "max", "letter", "password"), 
          "(\\d+)-(\\d+) (.): *(.*)", convert = TRUE)

# part 1 solution
# how many passwords are valid according to their policies?
passwords %>% 
  mutate(letter_count = map2_int(password, letter, str_count)) %>% 
  filter(letter_count >= min, letter_count <= max) %>% 
  nrow()

# part 2 solution
# how many passwords are valid according to the new policy interpretation?
passwords %>% 
  mutate(letter_count = (str_sub(password, min, min) == letter) +
           (str_sub(password, max, max) == letter)
  ) %>% 
  filter(letter_count == 1) %>% 
  nrow()
