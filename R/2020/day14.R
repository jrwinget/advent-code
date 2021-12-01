library(tidyverse)

# read data
input <- tibble(x = read_lines("data/day14.txt")) %>% 
  extract(x, "mask", "mask = (.*)", remove = FALSE, convert = TRUE) %>% 
  extract(x, c("address", "value"), c("mem\\[(.*)\\] = (.*)"), remove = FALSE, convert = TRUE) %>% 
  fill(mask) %>% 
  filter(!is.na(address))

# bits
powers <- 2 ^ seq(35, 0)

# function to change integer into bit
as_binary <- function(x) {
  c(rep(0L, 4), rev(as.integer(intToBits(x))))
}

# program instructions
instructions <- input %>% 
  mutate(
    binary_val = map(value, as_binary),
    mask = map(str_split(mask, ""), as.integer),
    masked = map2(mask, binary_val, coalesce),
    masked_val = map_dbl(masked, ~ sum(powers * .))
  )

# part 1 solution
addresses <- numeric(max(instructions$address))

# run the program
for(i in seq_len(nrow(instructions))) {
  addresses[instructions$address[i]] <- instructions$masked_val[i]
}

# part 1 solution
# what's the sum of all values left in memory after the program completes?
sum(addresses)

# function to consider possible floating values
floating <- function(mask) {
  # get number based on 1s
  n <- sum(powers * mask, na.rm = TRUE)
  
  # get vector of the 2 ^ floating possible values that could be added
  w <- which(is.na(mask))
  
  m <- sapply(seq_len(2 ^ length(w)) - 1, intToBits) %>% 
    head(length(w))
  
  n + c(powers[w] %*% (m == 1))
}

# program instructions
instructions <- instructions %>% 
  select(-binary_val, -masked) %>% 
  mutate(
    binary_address = map(address, as_binary),
    masked_address = map2(mask, binary_address, ~ if_else(is.na(.x), NA, .x | .y)),
    addresses = map(masked_address, floating)
  )

# keep addresses as named list
addresses <- list()

# run the program
for (i in seq_len(nrow(instructions))) {
  addresses[as.character(instructions$addresses[[i]])] <- instructions$value[i]
}

# part 2 solution
# what's the sum of all values left in memory after the program completes?
sum(unlist(addresses))
