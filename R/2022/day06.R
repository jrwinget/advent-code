# read data
input <- readLines("data/2022/day06.txt") |>
  strsplit("")

# part 1 solution
# how many characters are processed before the first SOP marker is detected?
for(i in seq_len(length(input[[1]]) - 3)) {
  if(length(unique(input[[1]][seq(i, i + 3)])) == 4) {
    break
  }
}
i + 3

# part 2 solution
# how many characters are processed with the updated SOP marker?
for(i in seq_len(length(input[[1]]) - 13)) {
  if(length(unique(input[[1]][seq(i, i + 13)])) == 14) {
    break
  }
}
i + 13
