input <- scan("data/2021/day07.txt", sep = ",")

# part 1
# determine the horizontal position that the crabs can align to using the least 
# fuel possible. how much fuel must they spend to align to that position?
sum(abs(input - median(input)))

# part 2
# turns out, crab fuel burns linearly. determine the new horizontal position
# that uses the least fuel possible. how much fuel must they spend to align?
min_fuel <- function(x) {
  n <- abs(input - x)
  sum(n / 2 * (1 + n))
}

min_fuel(round(optimize(min_fuel, range(input))$min))
