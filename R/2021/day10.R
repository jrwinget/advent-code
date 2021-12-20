input <- strsplit(readLines("data/2021/day10.txt"), "")

# part 1
# find the 1st illegal character in each corrupted line of the navigation 
# subsystem. what is the total syntax error score for those errors?
pts <- list(
  "(" = -3, ")" = 3, 
  "[" = -57, "]" = 57, 
  "{" = -1197, "}" = 1197, 
  "<" = -25137, ">" = 25137
)

chk_line <- function(x) {
  line_pts <- pts[x]
  n <- length(line_pts)
  i <- 2
  while(i <= n) {
    if(line_pts[[i]] > 0) {
      if(line_pts[[i]] != -line_pts[[i - 1]]) {
        return(line_pts[[i]])
      }
      line_pts[(i - 1):i] <- NULL
      i <- i - 1
      n <- n - 2
    } else {
      i <- i + 1
    }
  }
  0
}

scores <- sapply(input, chk_line)
sum(scores)

# part 2
# find the completion string for each incomplete line, score the completion 
# strings, and sort the scores. what is the middle score?
input <- input[scores == 0]

pts <- list(
  "(" = 1, ")" = -1, 
  "[" = 2, "]" = -2, 
  "{" = 3, "}" = -3, 
  "<" = 4, ">" = -4
)

fill_line <- function(x) {
  line_pts <- pts[x]
  n <- length(line_pts)
  i <- 2
  while(i <= n) {
    if(line_pts[[i]] < 0) {
      line_pts[(i - 1):i] <- NULL
      i <- i - 1
      n <- n - 2
    } else {
      i <- i + 1
    }
  }
  sum(unlist(line_pts) * 5^(seq_along(line_pts) - 1))
}

scores <- sapply(input, fill_line)
median(scores)
