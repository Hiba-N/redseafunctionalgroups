library(tidyverse)

#reading in data
red_sea_traits <- read_csv(
  "C:/Users/nasirh/OneDrive - KAUST/Documents/Data/red_sea_traits.csv"
)


#find percentage missing in each column

missing_percentage <- red_sea_traits %>%
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x)) * 100
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) %>%
  arrange(desc(percent_missing))

print(
  missing_percentage,
  n = Inf
)