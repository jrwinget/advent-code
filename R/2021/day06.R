# part 1
# simulate lanternfish & count total # of lanternfish after 80 days
input <- scan("data/2021/day06.txt", sep = ",")

for (i in seq_len(80)) {
  if (any(input == 0)) {
    input <- c(input, rep(9, sum(input == 0)))
  }
  input <- input - 1
  input[input == -1] <- 6
}

length(input)

# part 2
input <- table(scan("data/2021/day06.txt", sep = ","))
input[setdiff(as.character(0:8), names(input))] <- 0
input <- input[sort(names(input))]

for (i in seq_len(256)) {
  x <- input["0"]
  names(input) <- names(input)[c(length(names(input)), 1:length(names(input)) - 1)]
  input["6"] <- x + input["6"]
}

sum(input)
