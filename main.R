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
library(PCAmixdata)




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

#deleting unnecessary columns (such as metadata) except for reference columns
red_sea_final <- remove_columns(
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

#fixing a few random values
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

trait_table$BodyShapeI_morphdat[
  trait_table$BodyShapeI_morphdat == "other (see remarks)"
] <- "other"

trait_table$Electrogenic_species[
  trait_table$Electrogenic_species == "Electrosensing only"
] <- "electrosensing only"

table(
  trait_table$BodyShapeI_morphdat,
  useNA = "ifany"
)

#option 1: simply delete species with missing data

#???

#option 2: knn

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
  "results/9-3-2026/gower_results.csv",
  row.names = FALSE
)


results_summary <- summarise_knn_results(
  results
)


write.csv(
  results_summary,
  "results/9-3-2026/gower_results_summary.csv",
  row.names = FALSE
)


best_parameters <- get_best_parameters(
  results
)

write.csv(
  best_parameters,
  "results/9-3-2026/knn_gower_best_parameters.csv",
  row.names = FALSE
)


traits_imputed <- impute_using_best_parameters(
  data = trait_table,
  best_parameters = best_parameters,
  exclude_columns = EXCLUDED_COLUMNS
)

missing_data <- calculate_missing_percentage(traits_imputed)

#trying for f1 weighted instead of micro  #do this later

#pca, before and after
pca_original_results <- run_pcamix_analysis(
  data = trait_table,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  ndim = 5,
  graph = FALSE
)

#pca, before and after
pca_inferred_results <- run_pcamix_analysis(
  data = traits_imputed,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  ndim = 2,
  graph = TRUE
)

pca_inferred_results$correlation_plot

#comparing inferred data with base models (averages and modes)

baseline_results <- run_baseline_models(
  data = trait_table,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  mask_proportion = 0.10,
  seed = 123
)

write.csv(
  baseline_results ,
  "results/9-6-2026/baseline_results.csv",
  row.names = FALSE
)

#manual comparison done, consider doing a technical one | knn works better overall

#continuous to discrete

discrete_class_counts <- get_class_counts(
  data = traits_imputed,
  discrete_traits
)

View(discrete_class_counts)

#apart from binary flags we usually have classes 4,5,7,8 so we should go for around 6 classes

#plotting histograms
plot_continuous_distributions(
  data = traits_imputed,
  columns = continuous_traits
)

#discretizing according to literature
traits_half_discrete <- discretize_traits(traits_imputed)

#deleting unwanted columns
traits_half_discrete <- remove_columns(
  traits_half_discrete,
  UNUSED_TRAIT_COLUMNS
)

#plotting histograms
plot_continuous_distributions(
  data = traits_half_discrete,
  columns = REMAINDER_CONTINUOUS_TRAITS,
  bins = 40,
  n_breaks=4
)

