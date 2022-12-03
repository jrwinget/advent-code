# read data
input <- sapply(readLines("data/2022/day03.txt"), \(x) strsplit(x, ""))

# part 1 solution
# find item type in both compartments of the rucksacks and sum their priorities
in_both <- sapply(
  input, 
  \(x) intersect(x[1:length(x) / 2], x[(length(x) / 2 + 1):length(x)])
)

sum(match(in_both, c(letters, LETTERS)))

# part 2 solution
# find item type of the badges of each three-elf group and sum their priorities
grp_id <- rep(1:(length(input)/3), each = 3)
grp_lst <- split(input, grp_id)
in_all <- sapply(grp_lst, \(x) Reduce(intersect, x))

sum(match(in_all, c(letters, LETTERS)))
