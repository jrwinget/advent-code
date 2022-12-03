library(tidyverse)

input <- read.delim("data/2022/day02.txt", sep = " ", header = FALSE) |> 
  transform(V1 = match(V1, LETTERS), V2 = match(V2, LETTERS) - 23)

# part 1 solution
# what is total score if everything follows your strategy guide?
sum(with(input, V2 + ((1 + V2 - V1) %% 3) * 3))

# part 2 solution
# what is the total score if everything follows the updated strategy guide?
sum(with(input, ((V1 + V2) %% 3) + 1 + 3 * (V2 - 1)))
