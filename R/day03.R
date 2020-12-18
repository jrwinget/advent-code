library(tidyverse)

# read data
input <- read_lines("data/day03.txt")

# initialize parameters
lat <- lon <- 1
count <- 0

# for loop that updates count, lat, and lon based on slope
for (lon in 1:length(input)) {
  line <- str_dup(input[[lon]], ceiling(lat / str_length(input[[lon]])))
  count <- count + as.numeric(str_sub(line, lat, lat) == "#")
  lat <- lat + 3
  lon <- lon + 1
}

# part 1 solution
# starting at the top-left corner of the map and following a slope of 
# right 3 and down 1, how many trees would you encounter?
count


# determine the number of trees you would encounter for a series of slopes if
# you start at the top-left corner and traverse the map to the bottom
trees <- map2_dbl(
  c(1, 1, 1, 1, 2),
  c(1, 3, 5, 7, 1),
  ~ {
    lat <- lon <- 1
    count <- 0
    
    while(lon <= length(input)) {
      line <- str_dup(input[[lon]], ceiling(lat / str_length(input[[lon]])))
      count <- count + as.numeric(str_sub(line, lat, lat) == "#")
      lat <- lat + .y
      lon <- lon + .x
    }
    count
  }
)

# part 2 solution
# then find the product of the number of trees encountered for each slope
prod(trees)
