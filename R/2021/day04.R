# part 1
# find winning bingo board, sum all unmarked numbers, then multiply that sum by 
# the number that was just called when the board won to get the final score
numbers <- as.numeric(strsplit(readLines("data/2021/day04.txt"), ",")[[1]])
boards <- read.table("data/2021/day04.txt", skip = 2)

score <- function(x, draw) {
  marked <- is.na(x)
  if (all(c(rowMeans(marked), colMeans(marked)) != 1)) {
    return(0)
  }
  sum(x, na.rm = TRUE) * draw
}

find_winning <- function(numbers, boards) {
  size <- ncol(boards)
  n_boards <- nrow(boards) / size
  id <- rep(1:n_boards, each = size)
  for (i in numbers) {
    boards[boards == i] <- NA
    final_score <- sapply(split(boards, id), score, draw = i)
    if (any(final_score > 0)) {
      return(final_score[final_score > 0])
    }
  }
}

find_winning(numbers, boards)

# part 2
# find bingo board to win last, then compute score w/ the same scoring procedure
find_losing <- function(numbers, boards) {
  size <- ncol(boards)
  for (i in numbers) {
    n_boards <- nrow(boards) / size
    id <- rep(1:n_boards, each = size)
    boards[boards == i] <- NA
    final_score <- sapply(split(boards, id), score, draw = i)
    if (any(final_score > 0)) {
      if (n_boards == 1) {
        return(final_score[final_score > 0])
      }
      boards <- boards[id %in% which(final_score == 0), ]
    }
  }
}

find_losing(numbers, boards)
