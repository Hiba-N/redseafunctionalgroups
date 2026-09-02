#knn

EXCLUDED_COLUMNS <- c(
  "FBname_species",
  "spec_code",
  "Species",
  "stock_code"
)

K_VALUES <- c(
  3,
  5,
  7,
  10,
  15,
  20
)

WEIGHTED_VALUES <- c(
  TRUE,
  FALSE
)

MASK_PROPORTIONS <- c(
  0.10,
  0.20,
  0.30
)


SEEDS <- c(
  123,
  456,
  789
)


#FINAL_K <- #7

#FINAL_WEIGHTED <- #TRUE

GOWER_METRIC <- "gower"