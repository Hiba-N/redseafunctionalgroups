library(rfishbase)
source("utils/model_utils.R")
source("utils/utils.R")
source("constants/constants.R")
source("constants/model_constants.R")
library(missForest)


###########data prep

#check all available tables
fb_tables()
#fishbase_families <- fb_tbl('families')


# load tables
load_tables(tables)

# standardize tables
standardize_tableids(tables)

# get families of concern
reef_species = read.csv("constants/reef_species.csv")

#create species column
reef_species <- reef_species %>%
  mutate(
    Species = sub("^[^ ]+ ", "", Genus.species)
  )

#get all fish
all_fish <- species()
all_fish <- all_fish %>%
  rename(spec_code = SpecCode)

#add species name directly to all_fish table
all_fish <- add_species_to_fish(all_fish, species)

#intersect all fish with needed families
reef_fish <- all_fish %>%
  semi_join(reef_species, by = "Species")

#sanity check on join (optional)
unique(reef_fish$Species)

#intersect tables with fish from red sea
all_reef_fish_tables <- intersect_tables(
  red_sea_fish = reef_fish,
  tables = tables
)

#rm(list = ls(pattern = "^redsea_"))

#for tables with repeating spec + stock codes take averages
average_tables(TABLES_TO_AVERAGE)
check_duplicate_ids(TABLES_TO_MERGE)

#merge all tables on the basis of spec code 
all_reef_fish_final <- merge_tables(
  reef_fish,
  TABLES_TO_MERGE
)

#sanity check 
unique(all_reef_fish_final$Species)

sapply(all_reef_fish_final[continuous_traits], class)
sapply(all_reef_fish_final[discrete_traits], class)

# Continuous traits → numeric
all_reef_fish_final[continuous_traits] <- lapply(
  all_reef_fish_final[continuous_traits],
  as.numeric
)

# Discrete traits → factors
all_reef_fish_final[discrete_traits] <- lapply(
  all_reef_fish_final[discrete_traits],
  as.factor
)

#find pec missing in each column
na_percentages <- calculate_na_percentage(all_reef_fish_final)
na_percentages

#remove columns with greater than x% missing data
all_reef_fish_final <- remove_high_na_columns(
  all_reef_fish_final,
  threshold = THRESHOLD
)

#deleting unnecessary columns (such as metadata) except for reference columns
all_reef_fish_final <- remove_columns(
  all_reef_fish_final,
  META_COLUMNS_TO_REMOVE
)

all_reef_fish_final <- remove_columns(
  all_reef_fish_final,
  ADDITIONAL_COLUMNS_TO_REMOVE
)


# Calculate missing percentage for all columns
missing_data <- calculate_missing_percentage(all_reef_fish_final)

table(
  all_reef_fish_final$BodyShapeI,
  useNA = "ifany"
)

names(all_reef_fish_final)
sapply(all_reef_fish_final, class)

#reassigning class types
all_reef_fish_final <- set_trait_classes(all_reef_fish_final)
sapply(all_reef_fish_final, class)

################inference

training_data <- all_reef_fish_final %>%
  dplyr::select(
    where(~ !is.character(.))
  )

# Train missForest
set.seed(789)
training_data <- as.data.frame(training_data)

class(training_data)

model <- missForest(
  training_data,
  ntree = 500,
  maxiter = 10,
  variablewise = TRUE,
  verbose = TRUE
)

# 1. Extract the imputed dataset
imputed_data <- model$ximp

# 2. Check the imputed data
str(imputed_data)
summary(imputed_data)

# 3. Check whether any missing values remain
sum(is.na(imputed_data))


# 5. Save the imputed dataset
write.csv(
  imputed_data,
  "results/10-5-2026 (reef species)/missForest_imputed_data.csv",
  row.names = FALSE
)

# 6. Save the missForest error results
write.csv(
  as.data.frame(model$OOBerror),
  "results/10-5-2026 (reef species)/missForests_results.csv",
  row.names = TRUE
)

error_table <- data.frame(
  Variable = names(training_data),
  ErrorType = names(model$OOBerror),
  MSE_or_PFC = as.numeric(model$OOBerror),
  Variance = sapply(training_data, function(x) {
    if (is.numeric(x) || is.integer(x)) {
      var(x, na.rm = TRUE)
    } else {
      NA
    }
  })
)

error_table$MSE_or_PFC <- format(
  error_table$MSE_or_PFC,
  scientific = FALSE,
  digits = 10
)

error_table$Variance <- format(
  error_table$Variance,
  scientific = FALSE,
  digits = 10
)

print(error_table)


#inference

# Copy the original data
missforest_imputed <- all_reef_fish_final

# Replace the missForest columns with the imputed values
missforest_imputed[names(training_data)] <- model$ximp

# Check the result
str(missforest_imputed)
sum(is.na(missforest_imputed))

# Save
write.csv(
  missforest_imputed,
  "results/10-5-2026 (reef species)/missForest_full_imputed_data.csv",
  row.names = FALSE
)

#mark red fish sea flag
red_sea_fish <- get_ecosystem_fish("Red Sea")

#change spec and stock codes to integers for the reef table
red_sea_fish <- get_ecosystem_fish("Red Sea") %>%
  dplyr::mutate(
    spec_code = as.character(spec_code),
    stock_code = as.character(stock_code)
  )

#mark red fish species in all fish data
missforest_imputed <- missforest_imputed %>%
  dplyr::left_join(
    red_sea_fish %>%
      dplyr::select(spec_code, stock_code) %>%
      dplyr::distinct() %>%
      dplyr::mutate(red_sea = TRUE),
    by = c("spec_code", "stock_code")
  ) %>%
  dplyr::mutate(
    red_sea = dplyr::coalesce(red_sea, FALSE)
  )

#pcaMix

library(PCAmixdata)
library(dplyr)

# Columns to keep OUT of the analysis
excluded <- c(
  "stock_code",
  "spec_code",
  "Species_species",
  "red_sea",
  "Species",
  "Genus"
)

# Start with all trait columns
pca_data <- missforest_imputed %>%
  dplyr::select(-all_of(excluded))

# Convert character columns to factors
pca_data <- pca_data %>%
  dplyr::mutate(
    dplyr::across(where(is.character), as.factor)
  )

# Check the variable classes
sapply(pca_data, class)

# Run PCAmix

# Create separate ordinary data.frames
X.quanti <- as.data.frame(
  pca_data %>% dplyr::select(where(is.numeric))
)

X.quali <- as.data.frame(
  pca_data %>% dplyr::select(where(is.factor))
)

# Check
class(X.quanti)
class(X.quali)

sapply(X.quanti, class)
sapply(X.quali, class)

pca_mix <- PCAmix(
  X.quanti = X.quanti,
  X.quali = X.quali,
  rename.level = TRUE,
  graph = FALSE
)


pca_mix$eig

head(pca_mix$ind$coord)

coordinates <- as.data.frame(pca_mix$ind$coord)

coordinates$Species_species <- missforest_imputed$Species_species
coordinates$red_sea <- missforest_imputed$red_sea

ggplot(coordinates, aes(x = `dim 1`, y = `dim 2`)) +
  
  # All fish in the background
  geom_point(
    data = coordinates %>% filter(red_sea == FALSE),
    color = "black",
    size = 1
  ) +
  
  # Red Sea fish on top
  geom_point(
    data = coordinates %>% filter(red_sea == TRUE),
    color = "red",
    size = 2
  ) +
  
  labs(
    x = paste0(
      "PCAmix Dimension 1 (",
      round(pca_mix$eig["dim 1", "Proportion"], 1),
      "%)"
    ),
    y = paste0(
      "PCAmix Dimension 2 (",
      round(pca_mix$eig["dim 2", "Proportion"], 1),
      "%)"
    )
  ) +
  
  theme_classic()

# ============================================================
# TABLE FOR DIMENSION 1
# ============================================================

dim1_numeric <- data.frame(
  Variable = rownames(pca_mix$quanti$contrib),
  Correlation = pca_mix$quanti.cor[, "dim 1"],
  Contribution = pca_mix$quanti$contrib[, "dim 1"]
)

dim1_numeric <- dim1_numeric[
  order(-abs(dim1_numeric$Correlation)),
]

dim1_numeric

# ============================================================
# CATEGORICAL VARIABLES - DIMENSION 1
# ============================================================

dim1_categorical <- data.frame(
  Variable = rownames(pca_mix$quali.eta2),
  Association_eta2 = pca_mix$quali.eta2[, "dim 1"]
)

dim1_categorical <- dim1_categorical[
  order(-dim1_categorical$Association_eta2),
]

dim1_categorical

# ============================================================
# TABLE FOR DIMENSION 2
# ============================================================

dim2_numeric <- data.frame(
  Variable = rownames(pca_mix$quanti$contrib),
  Correlation = pca_mix$quanti.cor[, "dim 2"],
  Contribution = pca_mix$quanti$contrib[, "dim 2"]
)

dim2_numeric <- dim2_numeric[
  order(-abs(dim2_numeric$Correlation)),
]

dim2_numeric

# ============================================================
# CATEGORICAL VARIABLES - DIMENSION 2
# ============================================================

dim2_categorical <- data.frame(
  Variable = rownames(pca_mix$quali.eta2),
  Association_eta2 = pca_mix$quali.eta2[, "dim 2"]
)

dim2_categorical <- dim2_categorical[
  order(-dim2_categorical$Association_eta2),
]

dim2_categorical
