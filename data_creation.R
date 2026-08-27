# Load libraries
#install.packages('rfishbase') #fishbase access
#install.packages('tidyverse') #data cleaning and joining
#install.packages('janitor') #data cleaning
#install.packages('knitr') #displaying data tables


# ============================================================
# LOAD PACKAGES
# ============================================================

library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)


# ============================================================
# CHECKING OUT FISHBASE TABLES
# ============================================================

fb_tables()

# Checking a single table
aquamaps <- fb_tbl("aquamaps")

# Checking table columns
cols <- colnames(aquamaps)
cols


# ============================================================
# FUNCTION: LOAD AND STANDARDIZE FISHBASE TABLE
# ============================================================

load_fb_table <- function(table_name) {
  
  fb_tbl(table_name) %>%
    janitor::clean_names() %>%
    rename(
      spec_code = any_of(c("spec_code", "speccode")),
      stock_code = any_of(c("stock_code", "stockcode"))
    )
}


# ============================================================
# GET ALL SPECIES / STOCKS IN THE RED SEA
# ============================================================

red_sea_fish <- species_by_ecosystem(
  ecosystem = "Red Sea"
) %>%
  janitor::clean_names() %>%
  rename(
    spec_code = any_of(c("spec_code", "speccode")),
    stock_code = any_of(c("stock_code", "stockcode"))
  )


# Check Red Sea keys
if (!"spec_code" %in% names(red_sea_fish)) {
  stop("red_sea_fish does not contain spec_code")
}

if (!"stock_code" %in% names(red_sea_fish)) {
  stop("red_sea_fish does not contain stock_code")
}


# ============================================================
# LOAD REQUIRED TABLES
# ============================================================

species <- load_fb_table("species")
ecology <- load_fb_table("ecology")
reproduction <- load_fb_table("reproduc")
eggs <- load_fb_table("eggs")
morphdat <- load_fb_table("morphdat")
stocks <- load_fb_table("stocks")
swimming <- load_fb_table("swimming")
popgrowth <- load_fb_table("popgrowth")
popr <- load_fb_table("pop_r")


# ============================================================
# SHORTLIST COLUMNS FROM TABLES
# ============================================================

species_traits <- species %>%
  select(
    spec_code,
    genus,
    species,
    f_bname,
    body_shape_i,
    demers_pelag,
    ana_cat,
    depth_range_shallow,
    depth_range_deep,
    depth_range_com_shallow,
    depth_range_com_deep,
    length,
    l_type_max_m,
    length_female,
    l_type_max_f,
    common_length,
    l_type_com_m,
    common_length_f,
    l_type_com_f,
    weight,
    weight_female,
    electrogenic
  )


ecology_traits <- ecology %>%
  select(
    spec_code,
    stock_code,
    
    # Feeding / trophic
    herbivory2,
    feeding_type,
    diet_troph,
    diet_se_troph,
    food_troph,
    food_se_troph,
    
    # General habitat
    neritic,
    supra_littoral_zone,
    saltmarshes,
    littoral_zone,
    tide_pools,
    intertidal,
    sub_littoral,
    oceanic,
    epipelagic,
    mesopelagic,
    bathypelagic,
    abyssopelagic,
    hadopelagic,
    estuaries,
    mangroves,
    marshes_swamps,
    
    # Habitat position
    benthic,
    sessile,
    mobile,
    demersal,
    endofauna,
    pelagic,
    
    # Schooling
    solitary,
    schooling,
    schooling_frequency,
    schooling_lifestage,
    shoaling,
    shoaling_frequency,
    shoaling_lifestage,
    
    # Substrate
    soft_bottom,
    sand,
    coarse,
    fine,
    silt,
    mud,
    ooze,
    detritus,
    organic,
    hard_bottom,
    rocky,
    rubble,
    gravel,
    
    # Special habitats
    macrophyte,
    sea_grass_beds,
    coral_reefs,
    reef_exclusive,
    reef_flats,
    lagoons,
    burrows,
    crevices,
    seamounts,
    cold_seeps,
    hydrothermal_vents,
    deep_water_corals
  )


reproduction_traits <- reproduction %>%
  select(
    spec_code,
    stock_code,
    repro_mode,
    fertilization,
    mating_system,
    monogamy_type,
    spawn_agg,
    spawning,
    batch_spawner,
    rep_guild1,
    rep_guild2,
    parental_care,
    rep_aquarium
  )


stocks_traits <- stocks %>%
  select(
    spec_code,
    stock_code,
    temp_min,
    temp_max,
    temp_preferred,
    temp_pref25,
    temp_pref50,
    temp_pref75,
    env_temp,
    resilience
  )


eggs_traits <- eggs %>%
  select(
    spec_code,
    stock_code,
    placeofdev
  )


morphdat_traits <- morphdat %>%
  select(
    spec_code,
    stock_code,
    typeof_eyes,
    posof_mouth,
    c_shape
  )


swimming_traits <- swimming %>%
  select(
    spec_code,
    adult_type
  )


popgrowth_traits <- popgrowth %>%
  select(
    spec_code,
    stock_code,
    tmax,
    k
  )


popr_traits <- popr %>%
  select(
    spec_code,
    stock_code,
    r
  )


# ============================================================
# FUNCTION: INTERSECT WITH RED SEA FISH
# ============================================================

make_red_sea_traits <- function(
    trait_table,
    by,
    select_cols = "spec_code"
) {
  
  red_sea_fish %>%
    select(all_of(select_cols)) %>%
    distinct() %>%
    left_join(
      trait_table,
      by = by
    )
}


# ============================================================
# CREATE RED SEA TRAIT TABLES
# ============================================================

# Species-level
red_sea_species_traits <- make_red_sea_traits(
  species_traits,
  by = "spec_code"
)


# Stock-level
red_sea_ecology_traits <- make_red_sea_traits(
  ecology_traits,
  by = "spec_code"
)


red_sea_reproduction_traits <- make_red_sea_traits(
  reproduction_traits,
  by = "spec_code"
)


red_sea_eggs_traits <- make_red_sea_traits(
  eggs_traits,
  by = "spec_code"
)


red_sea_morphdat_traits <- make_red_sea_traits(
  morphdat_traits,
  by = "spec_code"
)


red_sea_stocks_traits <- make_red_sea_traits(
  stocks_traits,
  by = "stock_code",
  select_cols = "stock_code"
)


# Species-level
red_sea_swimming_traits <- make_red_sea_traits(
  swimming_traits,
  by = "spec_code"
)


# Population tables
red_sea_popgrowth_traits <- make_red_sea_traits(
  popgrowth_traits,
  by = c("spec_code", "stock_code"),
  select_cols = c("spec_code", "stock_code")
)


red_sea_popr_traits <- make_red_sea_traits(
  popr_traits,
  by = c("spec_code", "stock_code"),
  select_cols = c("spec_code", "stock_code")
)


# ============================================================
# FUNCTION: AVERAGE MULTIPLE OBSERVATIONS WITHIN STOCK
# ============================================================

average_stock_observations <- function(trait_table) {
  
  trait_table %>%
    group_by(spec_code, stock_code) %>%
    summarise(
      across(
        where(is.numeric),
        ~ if (all(is.na(.x))) {
          NA_real_
        } else {
          mean(.x, na.rm = TRUE)
        }
      ),
      .groups = "drop"
    )
}


# ============================================================
# AVERAGE POPULATION TABLE OBSERVATIONS
# ============================================================

red_sea_popgrowth_traits <- red_sea_popgrowth_traits %>%
  average_stock_observations()


red_sea_popr_traits <- red_sea_popr_traits %>%
  average_stock_observations()





#save all tables

output_dir <- "C:/Users/nasirh/OneDrive - KAUST/Documents/Data"

write_csv(red_sea_species_traits,
          file.path(output_dir, "red_sea_species_traits.csv"))

write_csv(red_sea_ecology_traits,
          file.path(output_dir, "red_sea_ecology_traits.csv"))

write_csv(red_sea_reproduction_traits,
          file.path(output_dir, "red_sea_reproduction_traits.csv"))

write_csv(red_sea_eggs_traits,
          file.path(output_dir, "red_sea_eggs_traits.csv"))

write_csv(red_sea_morphdat_traits,
          file.path(output_dir, "red_sea_morphdat_traits.csv"))

write_csv(red_sea_stocks_traits,
          file.path(output_dir, "red_sea_stocks_traits.csv"))

write_csv(red_sea_swimming_traits,
          file.path(output_dir, "red_sea_swimming_traits.csv"))

write_csv(red_sea_popgrowth_traits,
          file.path(output_dir, "red_sea_popgrowth_traits.csv"))

write_csv(red_sea_popr_traits,
          file.path(output_dir, "red_sea_popr_traits.csv"))


#merging all tables

red_sea_traits <- list(
  red_sea_species_traits,
  red_sea_ecology_traits,
  red_sea_reproduction_traits,
  red_sea_eggs_traits,
  red_sea_morphdat_traits,
  red_sea_stocks_traits,
  red_sea_swimming_traits,
  red_sea_popgrowth_traits,
  red_sea_popr_traits
) %>%
  reduce(left_join, by = "spec_code")


#sanity check on final table

nrow(red_sea_traits)
n_distinct(red_sea_traits$spec_code)


#ensuring each species occurs once
red_sea_traits %>%
  count(spec_code) %>%
  filter(n > 1)


#saving final dataset

write_csv(
  red_sea_traits,
  file.path(output_dir, "red_sea_traits.csv")
)










