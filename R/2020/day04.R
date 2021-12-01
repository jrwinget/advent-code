library(tidyverse)

# read data
input <- tibble(x = read_lines("data/day04.txt"))
required <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

passports <- input %>% 
  mutate(passport = cumsum(x == "")) %>%
  separate_rows(x, sep = " ") %>% 
  separate(x, into = c("field", "value"), sep = ":") %>%
  filter(field %in% required)

# part 1 solution
# count the number of passports with all required fields
passports %>% 
  count(passport) %>% 
  summarize(solution = sum(n == 7))

# part 2 solution
# count the number of passports with all required fields and valid values
passports %>% 
  extract(value, c("height", "unit"), "(\\d+)(cm|in)", 
          convert = TRUE, remove = FALSE) %>% 
  mutate(
    valid = case_when(
      field == "byr" ~ between(as.integer(value), 1920, 2002),
      field == "iyr" ~ between(as.integer(value), 2010, 2020),
      field == "eyr" ~ between(as.integer(value), 2020, 2030),
      field == "hgt" ~ ifelse(unit == "cm", between(height, 150, 193),
                              between(height, 59, 76)),
      field == "hcl" ~ str_detect(value, "^#[0-9a-f]{6}$"),
      field == "ecl" ~ value %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
      field == "pid" ~ str_detect(value, "^[0-9]{9}$")
    )
  ) %>% 
  filter(valid) %>% 
  count(passport) %>% 
  summarize(solution = sum(n == 7))
