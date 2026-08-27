# Load libraries
install.packages('rfishbase') #fishbase access
install.packages('tidyverse') #data cleaning and joining
install.packages('janitor') #data cleaning
install.packages('knitr') #displaying data tables
library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)


###Checking out tables######
fb_tables()

#checking out single tables
aquamaps <- fb_tbl("aquamaps")

#checking table columns
cols <- colnames(aquamaps)
cols

#get all species in the red sea
red_sea_fish <- species_by_ecosystem(
  ecosystem = "Red Sea"
)%>% 
  janitor::clean_names()


#defining table loading function
load_fb_table <- function(table_name) {
  fb_tbl(table_name) %>%
    janitor::clean_names()
}


#loading required tables
species <- load_fb_table("species")
ecology <- load_fb_table("ecology")
reproduction <- load_fb_table("reproduc")
eggs <- load_fb_table("eggs")
morphdat <- load_fb_table("morphdat")
stocks <- load_fb_table("stocks")
swimming <- load_fb_table("swimming")
popgrowth <- load_fb_table("popgrowth")
popr <- load_fb_table("pop_r")


#shortlisting columns from tables

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
    speccode,
    placeofdev)

morphdat_traits <- morphdat %>%
  select(
    speccode,
    typeof_eyes,
    posof_mouth,
    c_shape)

swimming_traits <- swimming %>%
  select(
    spec_code,
    adult_type)

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

#function to intersect with red sea fishes table

make_red_sea_traits <- function(trait_table, by, select_cols = "spec_code") {
  
  red_sea_fish %>%
    select(all_of(select_cols)) %>%
    distinct() %>%
    left_join(
      trait_table,
      by = by
    )
}

#shortlisting tables with species from the red sea only

red_sea_species_traits <- make_red_sea_traits(
  species_traits,
  by = "spec_code"
)

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
  by = c("spec_code" = "speccode")
)

red_sea_morphdat_traits <- make_red_sea_traits(
  morphdat_traits,
  by = c("spec_code" = "speccode")
)

red_sea_stocks_traits <- make_red_sea_traits(
  stocks_traits,
  by = c("stockcode" = "stock_code"),
  select_cols = "stockcode"
)

red_sea_swimming_traits <- make_red_sea_traits(
  swimming_traits,
  by = "spec_code"
)

red_sea_popgrowth_traits <- make_red_sea_traits(
  popgrowth_traits,
  by = c(
    "spec_code" = "spec_code",
    "stockcode" = "stock_code"
  ),
  select_cols = c("spec_code", "stockcode")
)

red_sea_popr_traits <- make_red_sea_traits(
  popr_traits,
  by = c(
    "spec_code" = "spec_code",
    "stockcode" = "stock_code"
  ),
  select_cols = c("spec_code", "stockcode")
)


#for population tables, averaging across observations as well

red_sea_popgrowth_traits <- red_sea_fish %>%
  select(spec_code, stockcode) %>%
  distinct() %>%
  left_join(
    popgrowth_traits,
    by = c(
      "spec_code" = "spec_code",
      "stockcode" = "stock_code"
    )
  ) %>%
  group_by(spec_code, stockcode) %>%
  summarise(
    across(
      where(is.numeric),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


red_sea_popr_traits <- red_sea_fish %>%
  select(spec_code, stockcode) %>%
  distinct() %>%
  left_join(
    popr_traits,
    by = c(
      "spec_code" = "spec_code",
      "stockcode" = "stock_code"
    )
  ) %>%
  group_by(spec_code, stockcode) %>%
  summarise(
    across(
      where(is.numeric),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

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































ecology <- fb_tbl("ecology") %>% 
  janitor::clean_names()

list_fields("Trophic")
packageVersion("rfishbase")
ls("package:rfishbase")


# Join red_sea_fish with species info
red_sea_fish_w_traits <- left_join(red_sea_fish, species, by = "spec_code") 

write.csv(red_sea_fish_w_traits, "C:/Users/nasirh/OneDrive - KAUST/Documents/Data/red_sea_fish_w_traits.csv")


cols <- colnames(red_sea_fish_w_traits)
cols

#subset data as per required columns

red_sea_fish_w_required_traits <- red_sea_fish_w_traits[, 
                                                        c("species.x", "status", "current_presence",
                                                          "life_stage", "genus", "body_shape_i", "saltwater",
                                                          "demers_pelag", "air_breathing",
                                                          "ana_cat", "depth_range_shallow", "depth_range_deep",
                                                          "depth_range_com_shallow", "depth_range_com_deep",
                                                          "longevity_wild", "longevity_captive", "vulnerability",
                                                          "vulnerability_climate", "length", "l_type_max_m",
                                                          "length_female", "l_type_max_f", "common_length", "l_type_com_m",
                                                          "common_length_f", "l_type_com_f", "weight", "weight_female", "electrogenic")]
#use this to find all unique column values
unique(red_sea_fish_w_required_traits$life_stage)

filtered_red_sea <- red_sea_fish_w_required_traits %>%
  filter(
    !status %in% c("misidentification", "error"),
    current_presence != "Absent",
    saltwater == 1,
    life_stage == "adults" 
  )
                    
#columns with more than 25 pec missing values

library(dplyr)
df_clean <- filtered_red_sea %>% select(where(~ mean(is.na(.)) <= 0.25))
df_clean2 <- filtered_red_sea %>% select(where(~ mean(is.na(.)) <= 0.15))






library(tidyverse)
#install.packages('naniar') #fishbase access
library(naniar)

data(filtered_red_sea)
dat1 <- filtered_red_sea
miss_case_summary(dat1) %>% 
  filter(pct_miss >= 25)






