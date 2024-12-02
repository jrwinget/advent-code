# read data
input <- read.table("data/2024/day01.txt")

# part 1 solution
# what is the total distance between your lists?
sum(abs(sort((input)$V1) - sort(input$V2)))
