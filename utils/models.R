
# ============================================================
# model_utils.R
# Utility functions for kNN imputation and validation
# ============================================================

# Required packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(cluster)


# ------------------------------------------------------------
# 1. Prepare data for kNN
# ------------------------------------------------------------

prepare_knn_data <- function(data,
                             id_cols = c("spec_code", "stock_code"),
                             exclude_cols = NULL) {
  
  df <- data
  
  # Remove ID columns from predictors
  cols_to_remove <- intersect(
    c(id_cols, exclude_cols),
    names(df)
  )
  
  predictor_df <- df %>%
    select(-any_of(cols_to_remove))
  
  # Remove columns that are completely NA
  predictor_df <- predictor_df %>%
    select(where(~ !all(is.na(.))))
  
  # Convert character columns to factors
  predictor_df <- predictor_df %>%
    mutate(
      across(
        where(is.character),
        as.factor
      )
    )
  
  return(predictor_df)
}


# ------------------------------------------------------------
# 2. Identify variable types
# ------------------------------------------------------------

get_variable_types <- function(data) {
  
  tibble(
    variable = names(data),
    type = map_chr(
      data,
      ~ case_when(
        is.numeric(.x) ~ "continuous",
        is.factor(.x) ~ "categorical",
        is.logical(.x) ~ "categorical",
        is.character(.x) ~ "categorical",
        TRUE ~ "other"
      )
    )
  )
}


# ------------------------------------------------------------
# 3. Randomly mask known values
#
# We temporarily replace known values with NA.
# kNN then tries to recover them.
# ------------------------------------------------------------

mask_values <- function(data,
                        columns,
                        proportion = 0.20,
                        seed = 123) {
  
  set.seed(seed)
  
  masked_data <- data
  validation <- list()
  
  for (col in columns) {
    
    known_idx <- which(!is.na(data[[col]]))
    
    if (length(known_idx) < 5) {
      next
    }
    
    n_mask <- max(
      1,
      floor(length(known_idx) * proportion)
    )
    
    mask_idx <- sample(
      known_idx,
      n_mask
    )
    
    # Store original values
    validation[[col]] <- tibble(
      row_id = mask_idx,
      truth = data[[col]][mask_idx]
    )
    
    # Mask them
    masked_data[[col]][mask_idx] <- NA
  }
  
  list(
    data = masked_data,
    validation = validation
  )
}


# ------------------------------------------------------------
# 4. Gower distance
#
# Gower distance can handle:
# - continuous variables
# - categorical variables
# - mixed data
# - missing predictor values
# ------------------------------------------------------------

calculate_gower_distance <- function(data) {
  
  # daisy() handles mixed data automatically
  gower <- daisy(
    data,
    metric = "gower"
  )
  
  return(gower)
}


# ------------------------------------------------------------
# 5. Find nearest neighbours for one row
# ------------------------------------------------------------

find_neighbors <- function(gower_distance,
                           row_id,
                           k) {
  
  # Convert dist object to a matrix
  distance_matrix <- as.matrix(gower_distance)
  
  distances <- distance_matrix[row_id, ]
  
  # Remove the row itself
  distances[row_id] <- Inf
  
  # Only retain finite distances
  valid <- is.finite(distances)
  
  valid_ids <- which(valid)
  
  # If fewer than k valid neighbours exist,
  # use however many are available
  k_actual <- min(
    k,
    length(valid_ids)
  )
  
  if (k_actual == 0) {
    return(
      tibble(
        row_id = integer(0),
        distance = numeric(0)
      )
    )
  }
  
  neighbor_ids <- valid_ids[
    order(distances[valid_ids])[1:k_actual]
  ]
  
  neighbor_distances <- distances[
    neighbor_ids
  ]
  
  tibble(
    row_id = neighbor_ids,
    distance = neighbor_distances
  )
}


# ------------------------------------------------------------
# 6. Predict continuous variable
# ------------------------------------------------------------

predict_continuous <- function(neighbors,
                               data,
                               column,
                               weighted = TRUE) {
  
  values <- data[[column]][neighbors$row_id]
  
  valid <- !is.na(values)
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  values <- values[valid]
  distances <- neighbors$distance[valid]
  
  if (weighted) {
    
    # Avoid division by zero
    weights <- 1 / pmax(
      distances,
      1e-8
    )
    
    return(
      weighted.mean(
        values,
        weights
      )
    )
    
  } else {
    
    return(
      mean(
        values,
        na.rm = TRUE
      )
    )
  }
}


# ------------------------------------------------------------
# 7. Predict categorical variable
# ------------------------------------------------------------

predict_categorical <- function(neighbors,
                                data,
                                column,
                                weighted = TRUE) {
  
  values <- data[[column]][neighbors$row_id]
  
  valid <- !is.na(values)
  
  if (!any(valid)) {
    return(NA)
  }
  
  values <- as.character(
    values[valid]
  )
  
  distances <- neighbors$distance[valid]
  
  if (weighted) {
    
    weights <- 1 / pmax(
      distances,
      1e-8
    )
    
    weight_table <- tibble(
      value = values,
      weight = weights
    ) %>%
      group_by(value) %>%
      summarise(
        weight = sum(weight),
        .groups = "drop"
      ) %>%
      arrange(desc(weight))
    
    return(
      weight_table$value[1]
    )
    
  } else {
    
    return(
      names(
        sort(
          table(values),
          decreasing = TRUE
        )
      )[1]
    )
  }
}


# ------------------------------------------------------------
# 8. Calculate metrics for continuous variables
# ------------------------------------------------------------

calculate_continuous_metrics <- function(truth,
                                         prediction) {
  
  valid <- !is.na(truth) &
    !is.na(prediction)
  
  truth <- truth[valid]
  prediction <- prediction[valid]
  
  if (length(truth) == 0) {
    return(
      tibble(
        n = 0,
        rmse = NA_real_,
        mae = NA_real_,
        r2 = NA_real_
      )
    )
  }
  
  rmse <- sqrt(
    mean(
      (truth - prediction)^2
    )
  )
  
  mae <- mean(
    abs(truth - prediction)
  )
  
  if (length(unique(truth)) > 1) {
    
    r2 <- 1 -
      sum((truth - prediction)^2) /
      sum((truth - mean(truth))^2)
    
  } else {
    
    r2 <- NA_real_
  }
  
  tibble(
    n = length(truth),
    rmse = rmse,
    mae = mae,
    r2 = r2
  )
}


# ------------------------------------------------------------
# 9. Calculate metrics for categorical variables
# ------------------------------------------------------------

calculate_categorical_metrics <- function(truth,
                                          prediction) {
  
  valid <- !is.na(truth) &
    !is.na(prediction)
  
  truth <- as.character(truth[valid])
  prediction <- as.character(prediction[valid])
  
  if (length(truth) == 0) {
    
    return(
      tibble(
        n = 0,
        accuracy = NA_real_,
        balanced_accuracy = NA_real_
      )
    )
  }
  
  accuracy <- mean(
    truth == prediction
  )
  
  # Per-class recall
  classes <- unique(truth)
  
  recalls <- map_dbl(
    classes,
    function(class) {
      
      idx <- truth == class
      
      if (!any(idx)) {
        return(NA_real_)
      }
      
      mean(
        prediction[idx] == class
      )
    }
  )
  
  balanced_accuracy <- mean(
    recalls,
    na.rm = TRUE
  )
  
  tibble(
    n = length(truth),
    accuracy = accuracy,
    balanced_accuracy = balanced_accuracy
  )
}


# ------------------------------------------------------------
# 10. Calculate accuracy for one column
# ------------------------------------------------------------

evaluate_column <- function(truth,
                            prediction,
                            column_name) {
  
  if (is.numeric(truth)) {
    
    metrics <- calculate_continuous_metrics(
      truth,
      prediction
    )
    
    metrics %>%
      mutate(
        column = column_name,
        type = "continuous"
      ) %>%
      select(
        column,
        type,
        everything()
      )
    
  } else {
    
    metrics <- calculate_categorical_metrics(
      truth,
      prediction
    )
    
    metrics %>%
      mutate(
        column = column_name,
        type = "categorical"
      ) %>%
      select(
        column,
        type,
        everything()
      )
  }
}


# ------------------------------------------------------------
# 11. Overall experiment summary
# ------------------------------------------------------------

summarise_experiment <- function(results) {
  
  results %>%
    group_by(
      type
    ) %>%
    summarise(
      across(
        where(is.numeric),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


# ============================================================
# model.R
# kNN imputation + validation
# ============================================================

library(dplyr)
library(purrr)
library(cluster)


# ------------------------------------------------------------
# 1. kNN prediction for a single column
# ------------------------------------------------------------

knn_predict_column <- function(data,
                               column,
                               k = 5,
                               weighted = TRUE) {
  
  # Rows where target is missing
  missing_rows <- which(
    is.na(data[[column]])
  )
  
  if (length(missing_rows) == 0) {
    return(data[[column]])
  }
  
  # ----------------------------------------------------------
  # Predictor data
  # ----------------------------------------------------------
  
  predictor_data <- data %>%
    select(
      -any_of(
        c(
          "spec_code",
          "stock_code",
          column
        )
      )
    )
  
  # Remove completely missing columns
  predictor_data <- predictor_data %>%
    select(
      where(~ !all(is.na(.)))
    )
  
  # Convert character variables to factors
  predictor_data <- predictor_data %>%
    mutate(
      across(
        where(is.character),
        as.factor
      )
    )
  
  # ----------------------------------------------------------
  # Calculate Gower distances
  # ----------------------------------------------------------
  
  gower <- cluster::daisy(
    predictor_data,
    metric = "gower"
  )
  
  # Convert to matrix once
  distance_matrix <- as.matrix(gower)
  
  # Determine target type
  is_continuous <- is.numeric(
    data[[column]]
  )
  
  result <- data[[column]]
  
  # ----------------------------------------------------------
  # Predict each missing value
  # ----------------------------------------------------------
  
  for (row_id in missing_rows) {
    
    distances <- distance_matrix[row_id, ]
    
    # Don't use the row itself
    distances[row_id] <- Inf
    
    # Only neighbours with a known target value
    candidate_rows <- which(
      !is.na(data[[column]]) &
        is.finite(distances)
    )
    
    if (length(candidate_rows) == 0) {
      
      result[row_id] <- NA
      
      next
    }
    
    # Number of neighbours actually available
    k_actual <- min(
      k,
      length(candidate_rows)
    )
    
    # Select nearest neighbours
    neighbor_ids <- candidate_rows[
      order(
        distances[candidate_rows]
      )[1:k_actual]
    ]
    
    neighbors <- tibble(
      row_id = neighbor_ids,
      distance = distances[neighbor_ids]
    )
    
    # --------------------------------------------------------
    # Continuous
    # --------------------------------------------------------
    
    if (is_continuous) {
      
      prediction <- predict_continuous(
        neighbors = neighbors,
        data = data,
        column = column,
        weighted = weighted
      )
      
      # --------------------------------------------------------
      # Categorical
      # --------------------------------------------------------
      
    } else {
      
      prediction <- predict_categorical(
        neighbors = neighbors,
        data = data,
        column = column,
        weighted = weighted
      )
    }
    
    result[row_id] <- prediction
  }
  
  result
}


# ------------------------------------------------------------
# 2. Impute all columns
# ------------------------------------------------------------

knn_impute <- function(data,
                       target_columns,
                       k = 5,
                       weighted = TRUE) {
  
  result <- data
  
  for (column in target_columns) {
    
    message(
      "Imputing: ",
      column
    )
    
    result[[column]] <- knn_predict_column(
      data = result,
      column = column,
      k = k,
      weighted = weighted
    )
  }
  
  result
}


# ------------------------------------------------------------
# 3. Validate one kNN run
#
# This hides known values, predicts them,
# and compares prediction vs truth.
# ------------------------------------------------------------

validate_knn <- function(data,
                         target_columns,
                         k = 5,
                         mask_proportion = 0.20,
                         weighted = TRUE,
                         seed = 123) {
  
  # ----------------------------------------------------------
  # Mask known values
  # ----------------------------------------------------------
  
  masked <- mask_values(
    data = data,
    columns = target_columns,
    proportion = mask_proportion,
    seed = seed
  )
  
  masked_data <- masked$data
  validation <- masked$validation
  
  # ----------------------------------------------------------
  # Predict masked values
  # ----------------------------------------------------------
  
  predictions <- masked_data
  
  for (column in target_columns) {
    
    predictions[[column]] <- knn_predict_column(
      data = masked_data,
      column = column,
      k = k,
      weighted = weighted
    )
  }
  
  # ----------------------------------------------------------
  # Evaluate every column
  # ----------------------------------------------------------
  
  results <- map_dfr(
    target_columns,
    function(column) {
      
      validation_column <- validation[[column]]
      
      if (is.null(validation_column)) {
        return(NULL)
      }
      
      truth <- validation_column$truth
      
      prediction <- predictions[[column]][
        validation_column$row_id
      ]
      
      evaluate_column(
        truth = truth,
        prediction = prediction,
        column_name = column
      )
    }
  )
  
  results %>%
    mutate(
      k = k,
      weighted = weighted,
      mask_proportion = mask_proportion,
      seed = seed
    ) %>%
    select(
      k,
      weighted,
      mask_proportion,
      seed,
      everything()
    )
}


# ------------------------------------------------------------
# 4. Run many parameter combinations
# ------------------------------------------------------------

run_knn_experiments <- function(data,
                                target_columns,
                                k_values = c(3, 5, 7, 10),
                                weighted_values = c(TRUE, FALSE),
                                mask_proportions = c(0.10, 0.20),
                                seeds = c(123, 456, 789)) {
  
  parameter_grid <- expand.grid(
    k = k_values,
    weighted = weighted_values,
    mask_proportion = mask_proportions,
    seed = seeds
  )
  
  all_results <- vector(
    "list",
    nrow(parameter_grid)
  )
  
  for (i in seq_len(nrow(parameter_grid))) {
    
    params <- parameter_grid[i, ]
    
    message(
      "\n====================================\n",
      "Run ", i,
      " / ", nrow(parameter_grid),
      "\n",
      "k = ", params$k,
      "\n",
      "weighted = ", params$weighted,
      "\n",
      "mask = ", params$mask_proportion,
      "\n",
      "seed = ", params$seed,
      "\n",
      "===================================="
    )
    
    all_results[[i]] <- validate_knn(
      data = data,
      target_columns = target_columns,
      k = params$k,
      weighted = params$weighted,
      mask_proportion = params$mask_proportion,
      seed = params$seed
    )
  }
  
  bind_rows(
    all_results
  )
}


# ------------------------------------------------------------
# 5. Choose the best parameter combination
# ------------------------------------------------------------

select_best_knn <- function(results) {
  
  # For categorical variables:
  # higher accuracy = better
  #
  # For continuous variables:
  # lower RMSE = better
  #
  # We calculate a standardized score per column.
  
  results %>%
    group_by(
      k,
      weighted,
      mask_proportion
    ) %>%
    summarise(
      categorical_accuracy = mean(
        accuracy[type == "categorical"],
        na.rm = TRUE
      ),
      
      continuous_rmse = mean(
        rmse[type == "continuous"],
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    arrange(
      desc(categorical_accuracy),
      continuous_rmse
    )
}


