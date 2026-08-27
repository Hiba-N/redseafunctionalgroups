library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
source("constants.R")

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
  
  for (table_name in tables) {
    
    df <- get(table_name, envir = .GlobalEnv)
    
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
    
    # Put standardized table back
    assign(
      table_name,
      df,
      envir = .GlobalEnv
    )
  }
  
  invisible(tables)
}