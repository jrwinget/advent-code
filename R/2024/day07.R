library(tidyverse)

# read the data
input <- tibble(line = read_lines("data/2024/day07.txt")) |>
  separate(line, into = c("test_value", "numbers"), sep = ": ") |>
  mutate(
    test_value = as.numeric(test_value),
    numbers    = str_split(numbers, " ")
  )

# helper functions
evaluate_ltr <- function(numbers, operators) {
  result <- numbers[1]
  for (i in seq_along(operators)) {
    if (operators[i] == "+") {
      result <- result + numbers[i+1]
    } else {
      result <- result * numbers[i+1]
    }
  }
  result
}

can_produce_test_value_explicit <- function(numbers_chr, target_num) {
  numbers <- as.numeric(numbers_chr)
  n <- length(numbers)
  if (n == 1) return(numbers[1] == target_num)
  
  patterns <- expand.grid(
    rep(list(c("+","*")), n - 1),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  colnames(patterns) <- paste0("op", seq_len(n-1))
  for (row_i in seq_len(nrow(patterns))) {
    operators <- unlist(patterns[row_i,], use.names = FALSE)
    result <- evaluate_ltr(numbers, operators)
    if (identical(result, target_num)) return(TRUE)
  }
  FALSE
}

# what is the total calibration result of the valid equations?
input |>
  rowwise() |>
  mutate(
    is_valid = can_produce_test_value_explicit(numbers, test_value)
  ) |>
  ungroup() |>
  filter(is_valid) |>
  summarize(total = sum(test_value))
