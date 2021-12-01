# read data
input <- as.numeric(readLines("data/2021/day01.txt"))

# part 1 solution
# count the number of times a depth measurement increases from the previous
# measurement
sum(diff(input) > 0)

# part 2 solution
# count the number of times a depth measurement increases using a 
# three-measurement sliding window
sum(diff(input, 3) > 0)
