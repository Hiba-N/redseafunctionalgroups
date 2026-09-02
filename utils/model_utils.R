# ============================================================
# KNN IMPUTATION USING GOWER DISTANCE
# ============================================================

library(dplyr)
library(cluster)


# ============================================================
# HELPER FUNCTIONS
# ============================================================


# ------------------------------------------------------------
# MODE
# ------------------------------------------------------------

get_mode <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA)
  }
  
  values <- unique(x)
  
  values[
    which.max(
      tabulate(
        match(x, values)
      )
    )
  ]
}


# ------------------------------------------------------------
# IDENTIFY TRAIT COLUMNS
# ------------------------------------------------------------

get_trait_columns <- function(
    data,
    exclude_columns = NULL) {
  
  trait_columns <- setdiff(
    names(data),
    exclude_columns
  )
  
  return(trait_columns)
}


# ============================================================
# GOWER DISTANCE
# ============================================================


calculate_gower_distance <- function(
    data,
    predictor_columns) {
  
  predictors <- data[
    ,
    predictor_columns,
    drop = FALSE
  ]
  
  # Remove columns that contain no information
  usable <- sapply(
    predictors,
    function(x) {
      sum(!is.na(x)) > 0
    }
  )
  
  predictors <- predictors[
    ,
    usable,
    drop = FALSE
  ]
  
  if (ncol(predictors) == 0) {
    
    stop(
      "No usable predictor columns remain."
    )
  }
  
  # Gower handles mixed:
  # numeric
  # integer
  # factor
  # character
  # logical
  # data types
  
  gower <- daisy(
    predictors,
    metric = "gower"
  )
  
  return(gower)
}


# ============================================================
# IMPUTE ONE TRAIT
# ============================================================


impute_knn_column <- function(
    data,
    target,
    k = 5,
    weighted = TRUE,
    exclude_columns = NULL) {
  
  
  # ----------------------------------------------------------
  # Find observations
  # ----------------------------------------------------------
  
  known_idx <- which(
    !is.na(data[[target]])
  )
  
  missing_idx <- which(
    is.na(data[[target]])
  )
  
  
  # Nothing to impute
  if (length(missing_idx) == 0) {
    
    return(data[[target]])
  }
  
  
  # Nothing available to learn from
  if (length(known_idx) == 0) {
    
    warning(
      paste(
        "No known values available for:",
        target
      )
    )
    
    return(data[[target]])
  }
  
  
  # ----------------------------------------------------------
  # Predictor columns
  # ----------------------------------------------------------
  
  predictor_columns <- setdiff(
    names(data),
    c(
      exclude_columns,
      target
    )
  )
  
  
  # Remove completely missing predictors
  predictor_columns <- predictor_columns[
    sapply(
      data[predictor_columns],
      function(x) {
        !all(is.na(x))
      }
    )
  ]
  
  
  if (length(predictor_columns) == 0) {
    
    warning(
      paste(
        "No predictors available for:",
        target
      )
    )
    
    return(data[[target]])
  }
  
  
  # ----------------------------------------------------------
  # Calculate Gower distance
  # ----------------------------------------------------------
  
  gower <- calculate_gower_distance(
    data = data,
    predictor_columns = predictor_columns
  )
  
  distance_matrix <- as.matrix(gower)
  
  
  # ----------------------------------------------------------
  # Output
  # ----------------------------------------------------------
  
  result <- data[[target]]
  
  
  # ----------------------------------------------------------
  # Predict each missing observation
  # ----------------------------------------------------------
  
  for (i in missing_idx) {
    
    
    # Distance from missing row
    # to all rows with known target
    
    distances <- distance_matrix[
      i,
      known_idx
    ]
    
    
    # Remove invalid distances
    
    valid <- !is.na(distances)
    
    distances <- distances[valid]
    
    neighbours <- known_idx[valid]
    
    
    if (length(neighbours) == 0) {
      
      next
    }
    
    
    # --------------------------------------------------------
    # Select K nearest neighbours
    # --------------------------------------------------------
    
    k_actual <- min(
      k,
      length(neighbours)
    )
    
    order_idx <- order(
      distances
    )[seq_len(k_actual)]
    
    neighbours <- neighbours[
      order_idx
    ]
    
    distances <- distances[
      order_idx
    ]
    
    
    neighbour_values <- data[
      neighbours,
      target
    ]
    
    
    # ========================================================
    # NUMERIC TARGET
    # ========================================================
    
    if (is.numeric(data[[target]])) {
      
      
      if (weighted) {
        
        # Inverse-distance weighting
        
        weights <- 1 /
          pmax(
            distances,
            1e-10
          )
        
        
        result[i] <- weighted.mean(
          neighbour_values,
          weights = weights,
          na.rm = TRUE
        )
        
        
      } else {
        
        result[i] <- mean(
          neighbour_values,
          na.rm = TRUE
        )
      }
      
      
      # ========================================================
      # CATEGORICAL TARGET
      # ========================================================
      
    } else {
      
      
      if (weighted) {
        
        # Weighted majority vote
        
        weights <- 1 /
          pmax(
            distances,
            1e-10
          )
        
        
        vote_table <- tapply(
          weights,
          neighbour_values,
          sum,
          na.rm = TRUE
        )
        
        
        result[i] <- names(
          vote_table
        )[
          which.max(vote_table)
        ]
        
        
      } else {
        
        # Ordinary majority vote
        
        result[i] <- get_mode(
          neighbour_values
        )
      }
    }
  }
  
  
  return(result)
}


# ============================================================
# IMPUTE ALL TRAITS
# ============================================================


impute_knn_gower <- function(
    data,
    target_columns = NULL,
    k = 5,
    weighted = TRUE,
    exclude_columns = NULL) {
  
  
  # ----------------------------------------------------------
  # Automatically determine traits
  # ----------------------------------------------------------
  
  if (is.null(target_columns)) {
    
    target_columns <- get_trait_columns(
      data = data,
      exclude_columns = exclude_columns
    )
  }
  
  
  result <- data
  
  
  # ----------------------------------------------------------
  # Impute each trait
  # ----------------------------------------------------------
  
  for (target in target_columns) {
    
    message(
      "Imputing: ",
      target,
      " | k = ",
      k,
      " | weighted = ",
      weighted
    )
    
    
    result[[target]] <- impute_knn_column(
      data = result,
      target = target,
      k = k,
      weighted = weighted,
      exclude_columns = exclude_columns
    )
  }
  
  
  return(result)
}


# ============================================================
# VALIDATION
# ============================================================


validate_knn_gower <- function(
    data,
    target_columns = NULL,
    k = 5,
    weighted = TRUE,
    mask_proportion = 0.20,
    seed = 123,
    exclude_columns = NULL) {
  
  
  # ----------------------------------------------------------
  # Determine traits automatically
  # ----------------------------------------------------------
  
  if (is.null(target_columns)) {
    
    target_columns <- get_trait_columns(
      data = data,
      exclude_columns = exclude_columns
    )
  }
  
  
  # ----------------------------------------------------------
  # Reproducibility
  # ----------------------------------------------------------
  
  set.seed(seed)
  
  
  # Keep original values
  original <- data
  
  
  # Create dataset where known values are artificially removed
  masked_data <- data
  
  
  # Store which values were hidden
  masked_positions <- list()
  
  
  # ==========================================================
  # MASK VALUES
  # ==========================================================
  
  for (target in target_columns) {
    
    
    known <- which(
      !is.na(data[[target]])
    )
    
    
    if (length(known) < 2) {
      
      next
    }
    
    
    n_mask <- floor(
      length(known) *
        mask_proportion
    )
    
    
    n_mask <- max(
      1,
      n_mask
    )
    
    
    n_mask <- min(
      n_mask,
      length(known) - 1
    )
    
    
    mask_idx <- sample(
      known,
      size = n_mask
    )
    
    
    masked_data[
      mask_idx,
      target
    ] <- NA
    
    
    masked_positions[[target]] <- mask_idx
  }
  
  
  # ==========================================================
  # IMPUTE
  # ==========================================================
  
  imputed <- impute_knn_gower(
    data = masked_data,
    target_columns = target_columns,
    k = k,
    weighted = weighted,
    exclude_columns = exclude_columns
  )
  
  
  # ==========================================================
  # METRICS
  # ==========================================================
  
  results <- list()
  
  
  for (target in target_columns) {
    
    
    idx <- masked_positions[[target]]
    
    
    if (is.null(idx)) {
      
      next
    }
    
    
    actual <- original[
      idx,
      target
    ][[1]]
    
    
    predicted <- imputed[
      idx,
      target
    ][[1]]
    
    
    # Remove failed predictions
    
    valid <- (
      !is.na(actual) &
        !is.na(predicted)
    )
    
    
    actual <- actual[valid]
    
    predicted <- predicted[valid]
    
    
    if (length(actual) == 0) {
      
      next
    }
    
    
    # ========================================================
    # NUMERIC
    # ========================================================
    
    if (is.numeric(original[[target]])) {
      
      
      rmse <- sqrt(
        mean(
          (actual - predicted)^2
        )
      )
      
      
      mae <- mean(
        abs(
          actual - predicted
        )
      )
      
      
      results[[target]] <- data.frame(
        
        target = target,
        
        type = "numeric",
        
        k = k,
        
        weighted = weighted,
        
        mask_proportion =
          mask_proportion,
        
        seed = seed,
        
        n_test =
          length(actual),
        
        accuracy = NA_real_,
        
        rmse = rmse,
        
        mae = mae,
        
        stringsAsFactors = FALSE
      )
      
      
      # ========================================================
      # CATEGORICAL
      # ========================================================
      
    } else {
      
      
      accuracy <- mean(
        actual == predicted
      )
      
      
      results[[target]] <- data.frame(
        
        target = target,
        
        type = "categorical",
        
        k = k,
        
        weighted = weighted,
        
        mask_proportion =
          mask_proportion,
        
        seed = seed,
        
        n_test =
          length(actual),
        
        accuracy = accuracy,
        
        rmse = NA_real_,
        
        mae = NA_real_,
        
        stringsAsFactors = FALSE
      )
    }
  }
  
  
  return(
    bind_rows(results)
  )
}


# ============================================================
# RUN ALL EXPERIMENTS
# ============================================================


run_knn_experiments <- function(
    data,
    exclude_columns = NULL,
    k_values = c(3, 5, 7, 10, 15),
    weighted_values = c(TRUE, FALSE),
    mask_proportions = c(0.10, 0.20),
    seeds = c(123, 456, 789)) {
  
  
  # ----------------------------------------------------------
  # Automatically determine traits
  # ----------------------------------------------------------
  
  target_columns <- get_trait_columns(
    data = data,
    exclude_columns = exclude_columns
  )
  
  
  message(
    "\nNumber of traits: ",
    length(target_columns)
  )
  
  
  message(
    "\nTraits being tested:"
  )
  
  
  print(target_columns)
  
  
  # ----------------------------------------------------------
  # Experiment combinations
  # ----------------------------------------------------------
  
  all_results <- list()
  
  counter <- 1
  
  
  # ==========================================================
  # LOOP
  # ==========================================================
  
  for (k in k_values) {
    
    for (weighted in weighted_values) {
      
      for (
        mask_prop in mask_proportions
      ) {
        
        for (seed in seeds) {
          
          
          message(
            "\n----------------------------------"
          )
          
          
          message(
            "Experiment: ",
            counter
          )
          
          
          message(
            "k = ",
            k
          )
          
          
          message(
            "weighted = ",
            weighted
          )
          
          
          message(
            "mask = ",
            mask_prop
          )
          
          
          message(
            "seed = ",
            seed
          )
          
          
          message(
            "----------------------------------"
          )
          
          
          result <- validate_knn_gower(
            
            data =
              data,
            
            target_columns =
              target_columns,
            
            k =
              k,
            
            weighted =
              weighted,
            
            mask_proportion =
              mask_prop,
            
            seed =
              seed,
            
            exclude_columns =
              exclude_columns
          )
          
          
          all_results[[counter]] <-
            result
          
          
          counter <- counter + 1
        }
      }
    }
  }
  
  
  return(
    bind_rows(all_results)
  )
}


# ============================================================
# SUMMARISE EXPERIMENTS
# ============================================================


summarise_knn_results <- function(
    results) {
  
  
  results %>%
    
    group_by(
      target,
      type,
      k,
      weighted,
      mask_proportion
    ) %>%
    
    summarise(
      
      mean_accuracy =
        mean(
          accuracy,
          na.rm = TRUE
        ),
      
      sd_accuracy =
        sd(
          accuracy,
          na.rm = TRUE
        ),
      
      mean_rmse =
        mean(
          rmse,
          na.rm = TRUE
        ),
      
      mean_mae =
        mean(
          mae,
          na.rm = TRUE
        ),
      
      n_tests =
        n(),
      
      .groups = "drop"
    )
}


# ============================================================
# FIND BEST PARAMETERS FOR EACH TRAIT
# ============================================================


get_best_parameters <- function(
    results) {
  
  
  summary <- summarise_knn_results(
    results
  )
  
  
  numeric_best <- summary %>%
    
    filter(
      type == "numeric"
    ) %>%
    
    group_by(
      target
    ) %>%
    
    slice_min(
      order_by = mean_rmse,
      n = 1,
      with_ties = FALSE
    )
  
  
  categorical_best <- summary %>%
    
    filter(
      type == "categorical"
    ) %>%
    
    group_by(
      target
    ) %>%
    
    slice_max(
      order_by = mean_accuracy,
      n = 1,
      with_ties = FALSE
    )
  
  
  bind_rows(
    numeric_best,
    categorical_best
  )
}

#imputation
impute_using_best_parameters <- function(
    data,
    best_parameters,
    exclude_columns = NULL) {
  
  result <- data
  
  for (i in seq_len(nrow(best_parameters))) {
    
    target <- best_parameters$target[i]
    k <- best_parameters$k[i]
    weighted <- best_parameters$weighted[i]
    
    message(
      "Imputing: ", target,
      " | k = ", k,
      " | weighted = ", weighted
    )
    
    # Only impute if the column exists
    if (!target %in% names(result)) {
      warning(
        "Target column not found in data: ",
        target
      )
      next
    }
    
    # Keep track of how many were missing before imputation
    missing_before <- sum(is.na(result[[target]]))
    
    if (missing_before == 0) {
      message("  No missing values.")
      next
    }
    
    # Impute using the best parameters for this trait
    result[[target]] <- impute_knn_column(
      data = result,
      target = target,
      k = k,
      weighted = weighted,
      exclude_columns = exclude_columns
    )
    
    missing_after <- sum(is.na(result[[target]]))
    
    message(
      "  Missing: ", missing_before,
      " -> ", missing_after
    )
  }
  
  return(result)
}