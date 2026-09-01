library(rfishbase)

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

remove_meta_columns <- function(df, columns_to_remove) {
  
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














