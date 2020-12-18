library(tidyverse)

# read data
instructions <- read_csv("data/day12.txt", col_names = "instructions") %>% 
  extract(instructions, c("action", "value"), "(.)(\\d+)", convert = TRUE)

# initialize ship parameters
lat <- lon <- 0
degree <- 90

# for loop that updates degree, lat, & lon based on each instruction
for (i in 1:nrow(instructions)) {
  action <- instructions$action[i]
  value <- instructions$value[i]
  
  degree <- case_when(
    action == "L" ~ (degree - value) %% 360,
    action == "R" ~ (degree + value) %% 360,
    TRUE ~ degree
  )
  
  lat <- case_when(
    action == "E" ~ lat + value,
    action == "W" ~ lat - value,
    action == "F" & degree == 90 ~ lat + value,
    action == "F" & degree == 270 ~ lat - value,
    TRUE ~ lat
  )
  
  lon <- case_when(
    action == "N" ~ lon + value,
    action == "S" ~ lon - value,
    action == "F" & degree == 0 ~ lon + value,
    action == "F" & degree == 180 ~ lon - value,
    TRUE ~ lon
  )
}

# part 1 solution
# what is the manhattan distance between end point and the ship's starting point
abs(lat) + abs(lon)

# initialize parameters
position <- 0 + 0i
waypoint <- 10 + 1i

# for loop that updates waypoint and position based on each instruction
for (i in 1:nrow(instructions)) {
  action <- instructions$action[i]
  value <- instructions$value[i]
  
  waypoint <- case_when(
    action == "L" ~ waypoint * complex(argument = (value / 180) * pi),
    action == "R" ~ waypoint * complex(argument = (-value / 180) * pi),
    action == "N" ~ waypoint + complex(real = 0, imaginary = value),
    action == "S" ~ waypoint + complex(real = 0, imaginary = -value),
    action == "E" ~ waypoint + complex(real = value, imaginary = 0),
    action == "W" ~ waypoint + complex(real = -value, imaginary = 0),
    TRUE ~ waypoint
  )
  
  if (action == "F") position <- position + waypoint * value
}

# part 2 solution
# what is the actual manhattan distance between end and starting points
abs(Re(position)) + abs(Im(position))
