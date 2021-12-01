library(tidyverse)

# read data
input <- tibble(x = read_lines("data/day13.txt"))
timestamp <- as.integer(input$x[1])
buses <- as.integer(str_split(input$x[2], ",")[[1]])

# part 1 solution
# what's the ID of the earliest bus you can take multiplied by 
# the number of mins you'll need to wait for that bus?
crossing(
  time = timestamp + seq(0, 100),
  bus = na.omit(buses)
) %>% 
  filter(time %% bus == 0) %>% 
  head(1) %>% 
  mutate(solution = (time - timestamp) * bus)

# given an index and modulo, change x such that (x + i) % m = 0
indicies <- which(!is.na(buses)) - 1
mod <- buses[indicies + 1]

x <- 0
size <- 1

# part 2 solution
# what's the earliest timestamp such that all of the listed bus IDs depart at
# offsets matching their positions in the list?
for (i in seq_along(indicies)) {
  remainder <- (x + size * seq(1, mod[i]) + indicies[i]) %% mod[i]
  x <- x + size * first(which(remainder == 0))
  size <- size * mod[i]
}

x
