library(tidyverse)

# read data
input <- tibble(x = readLines("data/2023/day04.txt"))

# part 1
# how many points are they worth in total?
n_matches <- input |> 
  extract(x, c("winning", "have"), ".*:( .*) \\| (.*)") |> 
  mutate(
    winning = str_extract_all(winning, "\\d+"),
    have = str_extract_all(have, "\\d+"),
    overlap = map2(winning, have, intersect),
    n_match = lengths(overlap)
  )

n_matches |> 
  filter(n_match > 0) |> 
  summarize(sum(2 ^(n_match-1)))

# part 2
# incl the original set, how many total scratchcards do you end up with?
n_games <- nrow(n_matches)
copies <- rep(1, n_games)

for(i in seq_len(n_games)) {
  if(n_matches$n_match[i] > 0) {
    range <- seq(i + 1, min(i + n_matches$n_match[i], n_games))
    copies[range] <- copies[range] + copies[i]
  }
}

sum(copies)
