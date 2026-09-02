library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
source("utils/utils.R")
source("constants/constants.R")
source("constants/model_constants.R")
library(readr)
library(writexl)
source("utils/model_utils.R")



#load tables
load_tables(tables)

#standardize tables
standardize_tableids(tables)

#get red sea fish
red_sea_fish <- get_ecosystem_fish("Red Sea")

#intersect tables with fish from red sea
redsea_tables <- intersect_redsea_tables(
  red_sea_fish = red_sea_fish,
  tables = tables
)

#continue processing only with tables that have around 1080 (75 pec) of entries left

#for tables with repeating spec + stock codes take averages
average_tables(TABLES_TO_AVERAGE)
check_duplicate_spec_codes(TABLES_TO_MERGE)

#continue with tables that have at least 700 rows left for now

#merge all tables on the basis of spec code 
red_sea_final <- merge_redsea_tables(
  red_sea_fish,
  TABLES_TO_MERGE
)

#find pec missing in each column
na_percentages <- calculate_na_percentage(red_sea_final)
na_percentages

#remove columns with greater than 25 missing data
red_sea_final <- remove_high_na_columns(
  red_sea_final,
  threshold = THRESHOLD
)

#deleting uneccessary columns (such as metadata) except for reference columns
red_sea_final <- remove_meta_columns(
  red_sea_final,
  META_COLUMNS_TO_REMOVE
)

# Calculate missing percentage for all columns
missing_data <- calculate_missing_percentage(red_sea_final)

trait_table <- create_trait_table(
  red_sea_final,
  selected_traits
)

missing_data <- calculate_missing_percentage(trait_table)

sapply(trait_table[continuous_traits], class)

plot_continuous_distributions(
  data = trait_table,
  columns = continuous_traits
)

sapply(trait_table[discrete_traits], class)

plot_discrete_distributions(
  data = trait_table,
  columns = discrete_traits
)

trait_table <- trait_table %>%
  mutate(
    Resilience_matrix = ifelse(
      tolower(trimws(Resilience_matrix)) == "please enter values for k, tmax.",
      NA,
      Resilience_matrix
    )
  )

trait_table[discrete_traits] <- lapply(
  trait_table[discrete_traits],
  as.factor
)


#knn

results <- run_knn_experiments(
  
  data =
    trait_table,
  
  exclude_columns =
    EXCLUDED_COLUMNS,
  
  k_values =
    K_VALUES,
  
  weighted_values =
    WEIGHTED_VALUES,
  
  mask_proportions =
    MASK_PROPORTIONS,
  
  seeds =
    SEEDS
)


write.csv(
  results,
  "results/2knn_gower_results.csv",
  row.names = FALSE
)


results_summary <- summarise_knn_results(
  results
)


write.csv(
  results_summary,
  "results/2knn_gower_results_summary.csv",
  row.names = FALSE
)


best_parameters <- get_best_parameters(
  results
)

write.csv(
  best_parameters,
  "results/2knn_gower_best_parameters.csv",
  row.names = FALSE
)


traits_imputed <- impute_using_best_parameters(
  data = trait_table,
  best_parameters = best_parameters,
  exclude_columns = EXCLUDED_COLUMNS
)

missing_data <- calculate_missing_percentage(traits_imputed)
