library(tidyverse)

# read data
input <- tibble(x = readLines("data/2023/day02.txt"))

# part 1 solution
# what is the sum of the IDs of those games?
game_cubes <- input |> 
  mutate(
    game = row_number(),
    cubes = str_extract_all(x, "\\d+ [a-z]+")
  ) |> 
  unnest(cubes) |> 
  separate(cubes, c("number", "color"), convert = TRUE)

total_cubes <- c(red = 12, green = 13, blue = 14)

game_cubes |> 
  mutate(total_cubes = total_cubes[color]) |> 
  group_by(game) |> 
  summarize(possible = !any(number > total_cubes)) |> 
  filter(possible) |> 
  summarize(sum(game))

# part 2 solution
# what is the sum of the power of these sets?
game_cubes |> 
  group_by(game, color) |> 
  summarize(number = max(number)) |> 
  summarize(power = prod(number)) |> 
  summarize(sum(power))
