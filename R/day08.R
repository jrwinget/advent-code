library(tidyverse)

# read data
boot_seq <- read_delim("data/day08.txt", " ", col_names = c("instruction", "value"))

# while loop function to sum accumulator after one boot sequence iteration
sum_accumulator <- function(df) {
  acc <- df$ran <- 0
  row <- 1
  
  repeat {
    df$ran[row] <- df$ran[row] + 1
    if (any(df$ran > 1)) break()
    acc <- acc + df$value[row] * (df$instruction[row] == "acc")
    row <- row + (df$instruction[row] =="jmp") * (df$value[row] - 1) + 1
  }
  
  nj <<- which(df$instruction != "acc" & df$ran) # needed for part 2
  print(acc)
}

# part 1 solution
# what value in the accumulator before the boot sequence iterates a second time?
sum_accumulator(boot_seq)

# for loop and while loop function that tries changes every allowed instruction
# function stops when boot sequence naturally terminates
check_all_the_instructions <- function(df) {
  for (i in nj) {
    modified_boot <- df
    
    # record which instruction wasn't used in initial boot sequence
    # only allowed to change one "nop" or one "jmp"
    modified_boot$instruction[i] <- setdiff(c("nop", "jmp"), df$instruction[i])
    
    acc <- modified_boot$ran <- 0
    row <- 1
    
    repeat {
      if (row == nrow(df) + 1)
        return(acc)
      
      modified_boot$ran[row] <- modified_boot$ran[row] + 1
      
      if (any(modified_boot$ran > 1))
        break
      
      acc <- acc + modified_boot$value[row] * (modified_boot$instruction[row] == "acc")
      row <- row + (modified_boot$instruction[row] == "jmp") * (modified_boot$value[row] - 1) + 1
    }
  }
}

# part 2 solution
# what is the value in the accumulator after the boot sequence terminates?
check_all_the_instructions(boot_seq)
