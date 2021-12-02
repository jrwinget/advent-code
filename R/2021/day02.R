# read data
input <- read.delim("data/2021/day02.txt", sep = " ", header = FALSE)

# part 1
# calculate the horizontal position and depth after following the planned course
# what is the product of the final horizontal position and depth?
hp <- 0
d <- 0

for (i in seq_len(nrow(input))) {
  command <- input[i, 1]
  unit <- input[i, 2]
  
  if (command == "forward") {
    hp <- hp + unit
  }
  
  if (command == "down") {
    d <- d + unit
  }
  
  if (command == "up") {
    d <- d - unit
  }
}

hp * d

# part 2
# using a new interpretation of the commands, what is the product of the final 
# horizontal position and depth?
hp <- 0
d <- 0
a <- 0

for (i in seq_len(nrow(input))) {
  command <- input[i, 1]
  unit <- input[i, 2]
  
  if (command == "forward") {
    hp <- hp + unit
    d <- d + (a * unit)
  }
  
  if (command == "down") {
    a <- a + unit
  }
  
  if (command == "up") {
    a <- a - unit
  }
}

hp * d
