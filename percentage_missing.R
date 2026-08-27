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


#deriving missing values from other stocks
# ============================================================
# FUNCTION: INFER MISSING VALUES FROM OTHER STOCKS
# ============================================================

infer_from_other_stocks <- function(
    trait_table,
    original_table,
    species_col = "spec_code",
    stock_col = "stock_code"
) {
  
  # ----------------------------------------------------------
  # Check required columns
  # ----------------------------------------------------------
  
  required_cols <- c(species_col, stock_col)
  
  if (!all(required_cols %in% names(trait_table))) {
    stop(
      "Missing columns in trait table: ",
      paste(
        setdiff(required_cols, names(trait_table)),
        collapse = ", "
      )
    )
  }
  
  if (!all(required_cols %in% names(original_table))) {
    stop(
      "Missing columns in original table: ",
      paste(
        setdiff(required_cols, names(original_table)),
        collapse = ", "
      )
    )
  }
  
  # ----------------------------------------------------------
  # Find trait columns present in both tables
  # ----------------------------------------------------------
  
  trait_cols <- intersect(
    names(trait_table),
    names(original_table)
  )
  
  trait_cols <- setdiff(
    trait_cols,
    c(species_col, stock_col)
  )
  
  # ----------------------------------------------------------
  # Process each trait
  # ----------------------------------------------------------
  
  for (col in trait_cols) {
    
    source_col <- paste0(col, "_source")
    
    # --------------------------------------------------------
    # Record whether existing value is original
    # --------------------------------------------------------
    
    trait_table[[source_col]] <- ifelse(
      is.na(trait_table[[col]]) |
        (is.character(trait_table[[col]]) &
           trimws(trait_table[[col]]) == ""),
      NA_character_,
      "original"
    )
    
    # --------------------------------------------------------
    # Process every row
    # --------------------------------------------------------
    
    for (i in seq_len(nrow(trait_table))) {
      
      value <- trait_table[[col]][i]
      
      # Check whether value is missing
      is_missing <- is.na(value) ||
        (is.character(value) &&
           trimws(value) == "")
      
      if (!is_missing) {
        next
      }
      
      # Current species and stock
      current_species <- trait_table[[species_col]][i]
      current_stock <- trait_table[[stock_col]][i]
      
      # ------------------------------------------------------
      # Find OTHER STOCKS of the SAME SPECIES
      # ------------------------------------------------------
      
      matches <-
        !is.na(original_table[[species_col]]) &
        original_table[[species_col]] == current_species &
        !is.na(original_table[[stock_col]]) &
        original_table[[stock_col]] != current_stock
      
      # ------------------------------------------------------
      # Candidate values
      # ------------------------------------------------------
      
      candidate_values <- original_table[[col]][matches]
      
      # Remove NA
      candidate_values <- candidate_values[
        !is.na(candidate_values)
      ]
      
      # Remove empty strings
      if (is.character(candidate_values)) {
        candidate_values <- candidate_values[
          trimws(candidate_values) != ""
        ]
      }
      
      # No usable values → leave as NA
      if (length(candidate_values) == 0) {
        next
      }
      
      # ------------------------------------------------------
      # NUMERIC → MEAN
      # ------------------------------------------------------
      
      if (is.numeric(candidate_values)) {
        
        inferred_value <- mean(
          candidate_values,
          na.rm = TRUE
        )
        
      } else {
        
        # ----------------------------------------------------
        # CATEGORICAL / TEXT → MODE
        # ----------------------------------------------------
        
        value_counts <- table(candidate_values)
        
        max_count <- max(value_counts)
        
        modes <- names(
          value_counts[value_counts == max_count]
        )
        
        # If tied, take the first
        inferred_value <- modes[1]
      }
      
      # ------------------------------------------------------
      # Replace missing value
      # ------------------------------------------------------
      
      trait_table[[col]][i] <- inferred_value
      
      trait_table[[source_col]][i] <- "derived"
    }
  }
  
  return(trait_table)
}
