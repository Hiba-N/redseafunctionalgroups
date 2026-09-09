library(rfishbase)
library(ggplot2)
library(PCAmixdata)
library(dplyr)
library(tidyr)
library(classInt)

#all tables
fb_tables()

#load a list of tables given
load_tables <- function(tables) {
  
  for (table in tables) {
    
    assign(
      table,
      fb_tbl(table),
      envir = .GlobalEnv #accessible globally instead of just in function
    )
    
  }
  
  invisible(tables) #don't print to console
}

#standardize stock_code and spec_code names
standardize_tableids <- function(tables) {
  
  # If a single data frame is supplied
  if (is.data.frame(tables)) {
    
    df <- tables
    
    # Standardize species code
    if ("SpecCode" %in% names(df)) {
      names(df)[names(df) == "SpecCode"] <- SPEC_CODE
    }
    
    if ("Speccode" %in% names(df)) {
      names(df)[names(df) == "Speccode"] <- SPEC_CODE
    }
    
    if ("speccode" %in% names(df)) {
      names(df)[names(df) == "speccode"] <- SPEC_CODE
    }
    
    # Standardize stock code
    if ("StockCode" %in% names(df)) {
      names(df)[names(df) == "StockCode"] <- STOCK_CODE
    }
    
    if ("Stockcode" %in% names(df)) {
      names(df)[names(df) == "Stockcode"] <- STOCK_CODE
    }
    
    if ("stockcode" %in% names(df)) {
      names(df)[names(df) == "stockcode"] <- STOCK_CODE
    }
    
    return(df)
  }
  
  
  # If a vector of table names is supplied
  for (table_name in tables) {
    
    df <- get(table_name, envir = .GlobalEnv)
    
    if ("SpecCode" %in% names(df)) {
      names(df)[names(df) == "SpecCode"] <- SPEC_CODE
    }
    
    if ("Speccode" %in% names(df)) {
      names(df)[names(df) == "Speccode"] <- SPEC_CODE
    }
    
    if ("speccode" %in% names(df)) {
      names(df)[names(df) == "speccode"] <- SPEC_CODE
    }
    
    if ("StockCode" %in% names(df)) {
      names(df)[names(df) == "StockCode"] <- STOCK_CODE
    }
    
    if ("Stockcode" %in% names(df)) {
      names(df)[names(df) == "Stockcode"] <- STOCK_CODE
    }
    
    if ("stockcode" %in% names(df)) {
      names(df)[names(df) == "stockcode"] <- STOCK_CODE
    }
    
    assign(
      table_name,
      df,
      envir = .GlobalEnv
    )
  }
  
  invisible(tables)
}

get_ecosystem_fish <- function(ecosystem) {
  
  # Get species from the specified ecosystem
  fish <- species_by_ecosystem(
    ecosystem = ecosystem
  )
  
  # Remove species where CurrentPresence is "Absent"
  if ("CurrentPresence" %in% names(fish)) {
    
    fish <- fish[
      is.na(fish$CurrentPresence) |
        tolower(fish$CurrentPresence) != "absent",
    ]
    
  }
  
  fish <- standardize_tableids(fish)
  
  return(fish)
}

intersect_redsea_tables <- function(red_sea_fish, tables) {
  
  redsea_tables <- list()
  
  for (table_name in tables) {
    
    # Get the table
    df <- get(table_name, envir = .GlobalEnv)
    
    # Check which ID columns are available in both tables
    has_spec <- "spec_code" %in% names(df) &&
      "spec_code" %in% names(red_sea_fish)
    
    has_stock <- "stock_code" %in% names(df) &&
      "stock_code" %in% names(red_sea_fish)
    
    
    # -------------------------
    # Match on BOTH IDs
    # -------------------------
    
    if (has_spec && has_stock) {
      
      redsea_df <- dplyr::semi_join(
        df,
        red_sea_fish,
        by = c("spec_code", "stock_code")
      )
      
    }
    
    
    # -------------------------
    # Match on StockCode only
    # -------------------------
    
    else if (has_stock) {
      
      redsea_df <- dplyr::semi_join(
        df,
        red_sea_fish,
        by = "stock_code"
      )
      
    }
    
    
    # -------------------------
    # Match on SpecCode only
    # -------------------------
    
    else if (has_spec) {
      
      redsea_df <- dplyr::semi_join(
        df,
        red_sea_fish,
        by = "spec_code"
      )
      
    }
    
    
    # -------------------------
    # No matching ID
    # -------------------------
    
    else {
      
      redsea_df <- df
      
      warning(
        paste(
          "No matching spec_code or stock_code found for:",
          table_name
        )
      )
      
    }
    
    
    # Name the resulting table
    redsea_name <- paste0("redsea_", table_name)
    
    
    # Store in list
    redsea_tables[[redsea_name]] <- redsea_df
    
    
    # Create individual object
    assign(
      redsea_name,
      redsea_df,
      envir = .GlobalEnv
    )
  }
  
  return(redsea_tables)
}

average_tables <- function(tables_to_average) {
  
  # Function to calculate the mode
  get_mode <- function(x) {
    
    x <- x[!is.na(x)]
    
    if (length(x) == 0) {
      return(NA)
    }
    
    names(which.max(table(x)))
  }
  
  
  # Loop through tables
  for (table_name in tables_to_average) {
    
    # Get table
    df <- get(table_name, envir = .GlobalEnv)
    
    
    # Determine grouping columns
    if (all(c("spec_code", "stock_code") %in% names(df))) {
      
      group_cols <- c("spec_code", "stock_code")
      
    } else if ("spec_code" %in% names(df)) {
      
      group_cols <- "spec_code"
      
    } else if ("stock_code" %in% names(df)) {
      
      group_cols <- "stock_code"
      
    } else {
      
      warning(
        paste(
          "No spec_code or stock_code found:",
          table_name
        )
      )
      
      next
    }
    
    
    # Columns to average / take mode
    value_cols <- setdiff(
      names(df),
      group_cols
    )
    
    
    # Collapse duplicate IDs
    df <- df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_cols)
        )
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(value_cols),
          ~ {
            
            if (is.numeric(.x)) {
              
              if (all(is.na(.x))) {
                NA_real_
              } else {
                mean(.x, na.rm = TRUE)
              }
              
            } else {
              
              get_mode(.x)
              
            }
          }
        ),
        .groups = "drop"
      )
    
    
    # Replace the original redsea_ object
    assign(
      table_name,
      df,
      envir = .GlobalEnv
    )
  }
  
  invisible(NULL)
}

merge_redsea_tables <- function(red_sea_fish, tables_to_merge) {
  
  # Start with Red Sea fish table
  merged_data <- red_sea_fish
  
  # Merge each table
  for (table_name in tables_to_merge) {
    
    df <- get(
      table_name,
      envir = .GlobalEnv
    )
    
    # Only merge if spec_code exists
    if (!"spec_code" %in% names(df)) {
      
      warning(
        paste(
          "Skipping", table_name,
          "- no spec_code column"
        )
      )
      
      next
    }
    
    # Get table name without "redsea_"
    suffix <- sub(
      "^redsea_",
      "",
      table_name
    )
    
    # Append table name to all columns except spec_code
    names(df) <- ifelse(
      names(df) == "spec_code",
      "spec_code",
      paste0(
        names(df),
        "_",
        suffix
      )
    )
    
    # Merge using spec_code
    merged_data <- merged_data |>
      dplyr::left_join(
        df,
        by = "spec_code"
      )
  }
  
  return(merged_data)
}

check_duplicate_spec_codes <- function(tables) {
  
  for (table_name in tables) {
    
    df <- get(table_name, envir = .GlobalEnv)
    
    if ("spec_code" %in% names(df)) {
      
      duplicates <- df |>
        dplyr::count(spec_code) |>
        dplyr::filter(n > 1)
      
      if (nrow(duplicates) > 0) {
        
        cat(
          "\n", table_name,
          "still has", nrow(duplicates),
          "spec_codes with multiple rows\n"
        )
      }
    }
  }
  
  invisible(NULL)
}

calculate_na_percentage <- function(df) {
  
  na_percentage <- sapply(
    df,
    function(x) {
      mean(is.na(x)) * 100
    }
  )
  
  na_percentage <- data.frame(
    column = names(na_percentage),
    na_percentage = as.numeric(na_percentage),
    row.names = NULL
  )
  
  return(na_percentage)
}

remove_high_na_columns <- function(df, threshold = 25) {
  
  na_percentage <- calculate_na_percentage(df)
  
  columns_to_remove <- na_percentage$column[
    na_percentage$na_percentage > threshold
  ]
  
  df <- df |>
    dplyr::select(
      -dplyr::all_of(columns_to_remove)
    )
  
  return(df)
}

remove_columns <- function(df, columns_to_remove) {
  
  df <- df |>
    dplyr::select(
      -dplyr::any_of(columns_to_remove)
    )
  
  return(df)
}

calculate_missing_percentage <- function(df) {
  
  missing_values <- tolower(MISSING_VALUES)
  
  missing_percentages <- sapply(df, function(x) {
    
    # Convert to character for consistent comparison
    x_char <- trimws(tolower(as.character(x)))
    
    # Count NA, empty cells, or specified missing values
    missing <- is.na(x) | x_char %in% missing_values
    
    # Calculate percentage
    mean(missing) * 100
  })
  
  # Create output table
  result <- data.frame(
    column = names(missing_percentages),
    missing_percentage = as.numeric(missing_percentages),
    row.names = NULL
  )
  
  # Print all column data
  print(result)
  
  return(result)
}


create_trait_table <- function(df, selected_traits) {
  
  trait_table <- df %>%
    dplyr::select(dplyr::any_of(selected_traits))
  
  return(trait_table)
}


map_missing_values <- function(df, column_a, column_b) {
  
  # Identify missing values in A
  a_missing <- is.na(df[[column_a]]) |
    trimws(tolower(as.character(df[[column_a]]))) %in% 
    tolower(MISSING_VALUES)
  
  # Identify non-missing values in B
  b_present <- !is.na(df[[column_b]]) &
    !(
      trimws(tolower(as.character(df[[column_b]]))) %in%
        tolower(MISSING_VALUES)
    )
  
  # Only map B → A when A is missing AND B is present
  df[[column_a]][a_missing & b_present] <- 
    df[[column_b]][a_missing & b_present]
  
  return(df)
}



# Plot distribution of continuous variables
plot_continuous_distributions <- function(data,
                                          columns = NULL,
                                          bins = 30) {
  
  library(ggplot2)
  
  # If no columns supplied, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  for (col in columns) {
    
    p <- ggplot(data, aes(x = .data[[col]])) +
      geom_histogram(
        bins = bins,
        na.rm = TRUE
      ) +
      labs(
        title = paste("Distribution of", col),
        x = col,
        y = "Count"
      ) +
      theme_minimal()
    
    print(p)
  }
}


plot_continuous_distributions <- function(data,
                                          columns = NULL,
                                          bins = 30,
                                          n_breaks = 4) {
  
  # If no columns supplied, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  for (col in columns) {
    
    # Remove NA values
    x <- data[[col]]
    x <- x[!is.na(x)]
    
    # Need enough unique values for natural breaks
    if (length(unique(x)) < n_breaks + 1) {
      message(
        "Skipping ", col,
        ": not enough unique values for ",
        n_breaks, " breaks."
      )
      next
    }
    
    # Calculate Jenks natural breaks
    breaks <- classInt::classIntervals(
      x,
      n = n_breaks,
      style = "jenks"
    )$brks
    
    # Plot
    p <- ggplot(data, aes(x = .data[[col]])) +
      geom_histogram(
        bins = bins,
        na.rm = TRUE
      ) +
      geom_vline(
        xintercept = breaks,
        linetype = "dashed",
        linewidth = 0.7
      ) +
      labs(
        title = paste("Distribution of", col),
        subtitle = paste(
          "Jenks natural breaks:",
          paste(round(breaks, 2), collapse = " | ")
        ),
        x = col,
        y = "Count"
      ) +
      theme_minimal()
    
    print(p)
  }
}



#Plot distribution of discrete / categorical variables
plot_discrete_distributions <- function(data,
                                        columns = NULL) {
  
  # If no columns supplied, use character, factor and logical columns
  if (is.null(columns)) {
    columns <- names(data)[
      sapply(
        data,
        function(x) is.character(x) ||
          is.factor(x) ||
          is.logical(x)
      )
    ]
  }
  
  for (col in columns) {
    
    p <- ggplot(data, aes(x = .data[[col]])) +
      geom_bar(na.rm = TRUE) +
      labs(
        title = paste("Distribution of", col),
        x = col,
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    print(p)
  }
}

# ============================================================
# PCAMIX + CORRELATION ANALYSIS
# ============================================================

run_pcamix_analysis <- function(data,
                                continuous_traits,
                                discrete_traits,
                                ndim = 2,
                                graph = TRUE) {
  
#pca functions
  
  
  all_pca_traits <- c(continuous_traits, discrete_traits)
  
  # Keep rows with complete continuous data
  #  pca_data <- data[
  #    complete.cases(data[, continuous_traits, drop = FALSE]),
  #    ,
  #    drop = FALSE
  #  ]
  
  # Keep rows with complete continuous data
  pca_data <- data[
    complete.cases(data[, all_pca_traits, drop = FALSE]),
    ,
    drop = FALSE
  ]

  
  continuous_table <- as.data.frame(
    pca_data[, continuous_traits, drop = FALSE]
  )
  
  # Explicitly convert continuous variables to numeric
  continuous_table[] <- lapply(
    continuous_table,
    function(x) as.numeric(as.character(x))
  )
  
  
  discrete_table <- as.data.frame(
    pca_data[, discrete_traits, drop = FALSE]
  )
  
  # Convert categorical variables to factors
  discrete_table[] <- lapply(
    discrete_table,
    as.factor
  )
  
  
  non_numeric <- names(continuous_table)[
    !sapply(continuous_table, is.numeric)
  ]
  
  if (length(non_numeric) > 0) {
    stop(
      paste(
        "The following continuous variables are not numeric:",
        paste(non_numeric, collapse = ", ")
      )
    )
  }
  
  
  # Check remaining NAs
  continuous_na <- colSums(is.na(continuous_table))
  discrete_na <- colSums(is.na(discrete_table))
  
  pcamix <- PCAmixdata::PCAmix(
    X.quanti = continuous_table,
    X.quali = discrete_table,
    rename.level = TRUE,
    graph = graph,
    ndim = ndim
  )
  
  
  cor_matrix <- cor(
    continuous_table,
    method = "pearson",
    use = "complete.obs"
  )
  
  
  cor_long <- as.data.frame(cor_matrix) %>%
    mutate(Trait1 = rownames(.)) %>%
    pivot_longer(
      cols = -Trait1,
      names_to = "Trait2",
      values_to = "Correlation"
    )
  
  
  # Preserve ordering
  cor_long$Trait1 <- factor(
    cor_long$Trait1,
    levels = rev(colnames(cor_matrix))
  )
  
  cor_long$Trait2 <- factor(
    cor_long$Trait2,
    levels = colnames(cor_matrix)
  )
  
  
  correlation_plot <- ggplot(
    cor_long,
    aes(
      x = Trait2,
      y = Trait1,
      fill = Correlation
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = sprintf("%.2f", Correlation)),
      size = 3
    ) +
    scale_fill_gradient2(
      low = "green",
      mid = "beige",
      high = "pink",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Corr"
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      panel.grid = element_blank()
    )
  
  
  return(
    list(
      pcamix = pcamix,
      pca_data = pca_data,
      continuous_table = continuous_table,
      discrete_table = discrete_table,
      correlation_matrix = cor_matrix,
      correlation_table = cor_long,
      correlation_plot = correlation_plot,
      continuous_NA = continuous_na,
      discrete_NA = discrete_na
    )
  )
}


# getting baseline models
get_mode <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA)
  }
  
  tab <- table(x)
  
  names(tab)[which.max(tab)]
}

run_baseline_models <- function(data,
                                continuous_traits,
                                discrete_traits,
                                mask_proportion = 0.10,
                                seed = 123) {
  
  set.seed(seed)
  
  original_data <- data
  masked_data <- data

  
  continuous_results <- lapply(
    continuous_traits,
    function(col) {
      
      x <- data[[col]]
      available <- which(!is.na(x))
      
      if (length(available) == 0) {
        return(data.frame(
          variable = col,
          type = "continuous",
          n_test = 0,
          baseline_prediction = NA,
          metric = "RMSE",
          value = NA
        ))
      }
      
      n_test <- max(
        1,
        floor(length(available) * mask_proportion)
      )
      
      test_indices <- sample(
        available,
        size = min(n_test, length(available))
      )
      
      # Mask test values
      masked_data[[col]][test_indices] <- NA
      
      # Mean from training/observed values
      baseline_prediction <- mean(
        masked_data[[col]],
        na.rm = TRUE
      )
      
      # True values
      true_values <- original_data[[col]][test_indices]
      
      # RMSE
      rmse <- sqrt(
        mean(
          (true_values - baseline_prediction)^2
        )
      )
      
      data.frame(
        variable = col,
        type = "continuous",
        n_test = length(test_indices),
        baseline_prediction = baseline_prediction,
        metric = "RMSE",
        value = rmse
      )
    }
  )
  
  
  continuous_results <- do.call(
    rbind,
    continuous_results
  )
  
  
  # ==========================================================
  # DISCRETE: MODE + ACCURACY
  # ==========================================================
  
  discrete_results <- lapply(
    discrete_traits,
    function(col) {
      
      x <- data[[col]]
      available <- which(!is.na(x))
      
      if (length(available) == 0) {
        return(data.frame(
          variable = col,
          type = "discrete",
          n_test = 0,
          baseline_prediction = NA,
          metric = "Accuracy",
          value = NA
        ))
      }
      
      n_test <- max(
        1,
        floor(length(available) * mask_proportion)
      )
      
      test_indices <- sample(
        available,
        size = min(n_test, length(available))
      )
      
      # Mask test values
      masked_data[[col]][test_indices] <- NA
      
      # Mode from training/observed values
      baseline_prediction <- get_mode(
        masked_data[[col]]
      )
      
      # True values
      true_values <- original_data[[col]][test_indices]
      
      # Accuracy
      accuracy <- mean(
        baseline_prediction == true_values,
        na.rm = TRUE
      )
      
      data.frame(
        variable = col,
        type = "discrete",
        n_test = length(test_indices),
        baseline_prediction = baseline_prediction,
        metric = "Accuracy",
        value = accuracy
      )
    }
  )
  
  
  discrete_results <- do.call(
    rbind,
    discrete_results
  )

  
  results <- rbind(
    continuous_results,
    discrete_results
  )
  
  
  # Add settings used for this experiment
  results$mask_proportion <- mask_proportion
  results$seed <- seed
  
  
  return(results)
}


#get unique class count per group

get_class_counts <- function(data, column_names) {
  
  unique_class_counts <- data.frame(
    variable = column_names,
    n_classes = sapply(
      data[column_names],
      function(x) length(unique(na.omit(x)))
    ),
    row.names = NULL
  )
  
  return(unique_class_counts)
}



#DISCRETIZE ONE COLUMN

discretize_column <- function(data,
                              column,
                              breaks,
                              labels) {
  
  if (!column %in% names(data)) {
    warning("Column not found: ", column)
    return(data)
  }
  
  if (length(breaks) != length(labels) + 1) {
    stop(
      "Number of breaks must be exactly one greater ",
      "than number of labels for column: ", column
    )
  }
  
  # Preserve NA values
  data[[column]] <- cut(
    data[[column]],
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE,
    ordered_result = FALSE
  )
  
  # Explicitly ensure factor
  data[[column]] <- factor(
    data[[column]],
    levels = labels
  )
  
  return(data)
}


# DISCRETIZE MULTIPLE COLUMNS USING DISCRETIZATION_RULES


discretize_traits <- function(data,
                              rules = DISCRETIZATION_RULES) {
  
  for (column in names(rules)) {
    
    if (!column %in% names(data)) {
      warning(
        "Discretization column not found in data: ",
        column
      )
      next
    }
    
    data <- discretize_column(
      data = data,
      column = column,
      breaks = rules[[column]]$breaks,
      labels = rules[[column]]$labels
    )
  }
  
  return(data)
}