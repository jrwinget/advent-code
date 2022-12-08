# read data
input <- strsplit(readLines("data/2022/day08.txt"), "")
dims <- length(input)
mat <- input |>
  unlist() |>
  as.numeric() |>
  matrix(byrow = TRUE, nrow = dims)

# tree scoring function
tree_score <- function(val, tree) {
  ifelse(
    any(val >= tree), 
    length(val[1:which(val >= tree)[1]]),
    length(val)
  )
}

# for loops to iterate through rows/cols
vis <- matrix(rep(TRUE, dims * dims), nrow = dims)
scenic <- matrix(rep(TRUE, dims * dims), nrow = dims)

for(i in 2:(dims - 1)) {
  for(j in 2:(dims - 1)) {
    tree <- mat[i, j]
    vis[i, j] <- any(
      c(
        all(mat[1:(i - 1), j] < tree),
        all(mat[(i + 1):dims, j] < tree),
        all(mat[i, 1:(j - 1)] < tree),
        all(mat[i, (j + 1):dims] < tree)
      )
    )
    scenic[i, j] <- prod(
      c(
        tree_score(rev(mat[1:(i - 1), j]), tree),
        tree_score(mat[(i + 1):dims, j], tree),
        tree_score(rev(mat[i, 1:(j - 1)]), tree),
        tree_score(mat[i, (j + 1):dims], tree)
      )
    )
  }
}

# part 1 solution
# how many trees are visible from outside the grid?
sum(vis)

# part 2 solution
# what is the highest scenic score possible for any tree?
max(scenic)
