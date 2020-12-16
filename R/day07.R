library(tidyverse)

# read data
input <- tibble(x = read_lines("data/day07.txt")) %>% 
  extract(x, c("bag_color", "contents"), "(.*) bags contain (.*)\\.")

luggage <- input %>% 
  separate_rows(contents, sep = ", ") %>% 
  extract(contents, c("number", "contents_color"), "([\\d]*) (.*) bag",
          convert = TRUE, remove = FALSE) %>% 
  filter(!is.na(number))

# part 1 solution
# takes a vector of colors and returns all bag_colors that contain that color
contained_by <- function(x, ...) {
  luggage %>% 
    filter(contents_color %in% x) %>% 
    pull(bag_color)
}

# how many bag colors can eventually contain at least one shiny gold bag?
accumulate(1:100, contained_by, .init = "shiny gold") %>% 
  tail(-1) %>%  # don't count the original shiny gold bag
  unlist() %>% 
  n_distinct()

# part 2 solution
# a recursive function that finds how many bags a bag contains
n_contain <- function(x) {
  rules <- luggage %>% 
    filter(bag_color == x)
  
  # each bag counts as 1, then all the bags it contains
  sum(rules$number * (1 + map_dbl(rules$contents_color, n_contain)))
}

# how many individual bags are required inside your single shiny gold bag?
n_contain("shiny gold")
