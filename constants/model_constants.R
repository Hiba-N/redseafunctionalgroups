#knn

EXCLUDED_COLUMNS <- c(
  "FBname_species",
  "spec_code",
  "Species_species",
  "stock_code"
)

K_VALUES <- c(
  3,
#  5,
  7,
#  10,
  15
#  20
)

WEIGHTED_VALUES <- c(
  TRUE,
  FALSE
)

MASK_PROPORTIONS <- c(
  0.10,
  # 0.20,
  0.30
)


SEEDS <- c(
  # 123,
  # 456,
  789
)


#FINAL_K <- #7

#FINAL_WEIGHTED <- #TRUE

GOWER_METRIC <- "gower"


MISSFOREST_COLUMNS <- c(
  "stock_code",
  "MaxLengthTL_estimate",
  "Troph_estimate",
  "PredPreyRatioMin_estimate",
  "PredPreyRatioMax_estimate",
  "FeedingPath_estimate",
  "MaxLengthSL_estimate",
  "mean_temp_matrix",
  "Life_span_matrix",
  "Generation_time_matrix",
  "tm_matrix",
  "Brack_species",
  "DemersPelag_species",
  "Vulnerability_species",
  "PD50_species",
  "EnvTemp_stocks",
  "spec_code",
  "Species_species"
)