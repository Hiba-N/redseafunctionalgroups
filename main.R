library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
source("utils.R")
source("constants.R")


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