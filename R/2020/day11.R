library(tidyverse)

# read data
seats <- read_lines("data/day11.txt") %>% 
  str_split("") %>% 
  map2_dfr(1:length(.), ~ tibble(val = .x, x = 1:length(.x), y = .y))

# returns x,y coords for surrounding seats (neighbors) based on given x,y coord
get_neighbors <- function(x = 1, y = 1) {
  expand_grid(neighbor_x = c(x - 1, x, x + 1), neighbor_y = c(y - 1, y, y + 1)) %>% 
    filter(!(neighbor_x == x & neighbor_y == y)) %>% 
    filter(
      neighbor_x > 0, neighbor_x <= max(seats$x), 
      neighbor_y > 0, neighbor_y <= max(seats$y)
    ) %>% 
    mutate(x = x, y = y)
}

# applies get_neighbors to every x,y coord in seating layout
layout <- map2_dfr(as.numeric(seats$x), as.numeric(seats$y), get_neighbors)

# counts initial adjacent occupied seats, applies rules
update_seats <- function(df, room = layout, min_occupied = 4) {
  df %>% 
    # adds neighbor_x and neighbor_y columns from layout
    inner_join(room, by = c("x", "y")) %>%
    # adds duplicate seating chart column
    inner_join(rename(df, val_a = val), by = c("neighbor_x" = "x", "neighbor_y" = "y")) %>%
    # groups by x,y grid coord
    group_by(x, y) %>%
    summarize(
      # counts number of adjacent occupied seats
      occ_neighor = sum(val_a == "#"),
      # preserves initial seating chart # of rows
      val = min(val),
      .groups = "drop"
    ) %>% 
    mutate(
      # creates new column for updated seats after 1 round
      new_val = case_when(
        # if seat is empty with no occupied neighbors, seat becomes occupied
        val == "L" & occ_neighor == 0 ~ "#",
        # if seat is occupied with 4+ occupied neighbors, seat becomes empty
        val == "#" & occ_neighor >= min_occupied ~ "L",
        # otherwise, the seat's state does not change
        TRUE ~ val
      )
    ) %>% 
    # arrange seating chart in initial order
    arrange(y, x)
}

# recursive function that runs the simulation
run_sim <- function(df, room = layout, min_occupied = 4) {
  df <- update_seats(df, room, min_occupied) # store updated seating chart after 1 iteration
  if (all(df$val == df$new_val)) return(df) # if there is no change in seats, return the data frame
  run_sim(select(df, val = new_val, x, y), room, min_occupied) # recursively run simulation until there is no change in seats
}

# part 1 solution
# simulate seating area by applying rules until no seats change state
# how many seats end up occupied?
run_sim(seats) %>% 
  summarize(solution = sum(new_val == "#"))

# create df that contains the seat and neighbors information
room <- inner_join(layout, seats, by = c("neighbor_x" = "x", "neighbor_y" = "y"))

# 
update_layout <- function(df) {
  # creates new df with new neighbor x,y coords based on new visibility rules
  new_room <- df %>% 
    mutate(
      new_nbr_x = if_else(val == ".", neighbor_x + sign(neighbor_x - x), neighbor_x),
      new_nbr_y = if_else(val == ".", neighbor_y + sign(neighbor_y - y), neighbor_y),
      # true if new nbr x,y is less than 1 or greater than original max coord
      invalid = new_nbr_x < 1 | new_nbr_x > max(df$x) | 
        new_nbr_y < 1 | new_nbr_y > max(df$y),
      # if any invalid cases, use last nbr x,y coord
      new_nbr_x = if_else(invalid, neighbor_x, new_nbr_x),
      new_nbr_y = if_else(invalid, neighbor_y, new_nbr_y),
      # true if new nbr x,y coords are the same as the last nbr x,y coords
      check_col = (new_nbr_x == neighbor_x) & (new_nbr_y == neighbor_y)
    )
  
  # saves current iteration of room layout
  room_iteration <- new_room %>% 
    select(
      x, y,
      neighbor_x = new_nbr_x,
      neighbor_y = new_nbr_y
    )
  
  # if there is no change in seats, return the current room iteration
  if (all(new_room$check_col)) return(room_iteration)
  
  # join the current iteration and seat df, then recursively update the layout
  inner_join(room_iteration, seats, by = c("neighbor_x" = "x", "neighbor_y" = "y")) %>% 
    update_layout()
}

# get updated room layout
latest_layout <- update_layout(room)

# part 2 solution
# simulate seating area by applying updated rules until no seats change state
# how many seats end up occupied?
run_sim(seats, latest_layout, 5) %>% 
  summarize(solution = sum(new_val == "#"))
