
#knn

target_columns <- c(
  "a_estimate",
  "sd_log10a_estimate",
  "b_estimate",
  "sd_b_estimate",
  "K_estimate",
  "ComDepthMin_estimate",
  "ComDepthMax_estimate",
  "DepthMin_estimate",
  "DepthMax_estimate",
  "PredPreyRatioMin_estimate",
  "PredPreyRatioMax_estimate",
  "TempPrefMin_estimate",
  "TempPrefMean_estimate",
  "TempPrefMax_estimate",
  "FeedingPath_estimate",
  "to_matrix",
  "Life_span_matrix",
  "Generation_time_matrix",
  "tm_matrix",
  "QB_matrix",
  "BodyShapeI_morphdat",
  "OperculumPresent_morphdat",
  "LLinterrupted_morphdat",
  "Notched_morphdat",
  "DorsalSoftRaysMin_morphdat",
  "DorsalSoftRaysMax_morphdat",
  "FBname_species",
  "AirBreathing_species",
  "PriceCateg_species",
  "Dangerous_species",
  "Electrogenic_species"
)

kvalues <- c(
  3,
  5,
  7,
  10,
  15
)

weighted_values <- c(
  TRUE,
  FALSE
)

mask_proportions <- c(
  0.10,
  0.20,
  0.30,
  0.40,
  0.50
)

seeds <- c(
  123,
  456,
  789
)

excluded_columns <- c(
  "spec_code",
  "stock_code",
  "FBname_species"
)