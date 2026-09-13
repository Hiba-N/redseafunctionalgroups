#SHORTLISTED COLUMNS

tables <- c(
  "airbreathingref",
  "alieninvasive",
  "diet",
  "diet_items",
  "ecology",
  "ecosystem",
  "eggdev",
  "eggnurserysystem",
  "eggs",
  "estimate",
  "estimatedepth",
  "fecundity",
  "food",
  "foodecosystemtype",
  "fooditems",
  "foodtroph",
  "gillarea",
  "larvae",
  "matrix",
  "maturity",
  "morphdat",
  "morphmet",
  "morphmettlratios",
  "myersdata",
  "pop_r",
  "popchar",
  "popgrowth",
  "poplf",
  "popll",
  "poplw",
  "popqb",
  "predatorecosystemtype",
  "predats",
  "ration",
  "reproduc",
  "sounds",
  "spawnagg",
  "spawning",
  "species",
  "speed",
  "stocks",
  "strains",
  "swimming"
)

# Standardized FishBase ID column names
SPEC_CODE <- "spec_code"
STOCK_CODE <- "stock_code"

TABLES_TO_AVERAGE <- c(
  "redsea_fooditems",
  "redsea_morphmet",
  "redsea_morphmettlratios",
  "redsea_popchar",
  "redsea_popgrowth",
  "redsea_popll",
  "redsea_poplw",
  "redsea_maturity"
)

TABLES_TO_MERGE <- c(
  "redsea_estimate",
  "redsea_fooditems",
  "redsea_matrix",
  "redsea_maturity",
  "redsea_morphdat",
  "redsea_morphmet",
  "redsea_morphmettlratios",
  "redsea_popchar",
  "redsea_popll",
  "redsea_poplw",
  "redsea_species",
  "redsea_stocks"
)

THRESHOLD <- 25

META_COLUMNS_TO_REMOVE <- c(
  "E_CODE",
  "autoctr",
  "EcosystemRefno",
  "Remarks",
  "Entered",
  "Dateentered",
  "Modified",
  "Datemodified",
  "LastModified_estimate",
  "ID_matrix",
  "FamCode_matrix",
  "Linf_comment_matrix",
  "autoctr_morphdat",
  "stock_code_matrix",
  "stock_code_morphdat",
  "Entered_morphdat",
  "DateEntered_morphdat",
  "DateModified_morphdat",
  "autoctr_morphmet",
  "PicName_morphmet",
  "Species_species",
  "Author_species",
  "PicPreferredName_species",
  "FamCode_species",
  "GenCode_species",
  "GoogleImage_species",
  "Entered_species",
  "DateEntered_species",
  "Modified_species",
  "DateModified_species",
  "stock_code_stocks",
  "SynOC_stocks",
  "Level_stocks",
  "IUCN_Code_stocks",
  "IUCN_DateAssessed_stocks",
  "Entered_stocks",
  "DateEntered_stocks",
  "Modified_stocks",
  "DateModified_stocks",
  "Reproductive_guild_matrix",
  "OperculumPresent_morphdat",
  "Notched_morphdat"
)

# Values to treat as missing
MISSING_VALUES <- c("", "unknown", "NA")

selected_traits <- c(
  "spec_code",
  "Species",
  "stock_code",
  "MaxLengthTL_estimate",
  "Troph_estimate",
  "seTroph_estimate",
  "a_estimate",
  "sd_log10a_estimate",
  "b_estimate",
  "sd_b_estimate",
  "K_estimate",
  "ComDepthMin_estimate",
  "ComDepthMax_estimate",
  "DepthMin_estimate",
  "DepthMax_estimate",
  "PredPreyRatioMin_estimate",
  "PredPreyRatioMax_estimate",
  "TempPrefMin_estimate",
  "TempPrefMean_estimate",
  "TempPrefMax_estimate",
  "FeedingPath_estimate",
  "MaxLengthSL_estimate",
  "to_matrix",
  "mean_temp_matrix",
  "Life_span_matrix",
  "Generation_time_matrix",
  "tm_matrix",
  "Resilience_matrix",
  "QB_matrix",
  "BodyShapeI_morphdat",
  "OperculumPresent_morphdat",
  "LLinterrupted_morphdat",
  "Notched_morphdat",
  "DorsalSoftRaysMin_morphdat",
  "DorsalSoftRaysMax_morphdat",
  "Genus_species",
  "FBname_species",
  "Fresh_species",
  "Brack_species",
  "DemersPelag_species",
  "AirBreathing_species",
  "Vulnerability_species",
  "Dangerous_species",
  "Electrogenic_species",
  "PD50_species",
  "EnvTemp_stocks"
)


y <- c(
  "MaxLengthTL_estimate",
  "Troph_estimate",
  "seTroph_estimate",
  "mean_temp_matrix",
  "E_matrix",
  "Resilience_matrix",
  "Genus_species",
  "Fresh_species",
  "Brack_species",
  "Saltwater_species",
  "DemersPelag_species",
  "Vulnerability_species",
  "PD50_species",
  "MaxLengthSL_estimate"
)

x <- c(
  "a_estimate",
  "sd_log10a_estimate",
  "b_estimate",
  "sd_b_estimate",
  "K_estimate",
  "ComDepthMin_estimate",
  "ComDepthMax_estimate",
  "DepthMin_estimate",
  "DepthMax_estimate",
  "PredPreyRatioMin_estimate",
  "PredPreyRatioMax_estimate",
  "TempPrefMin_estimate",
  "TempPrefMean_estimate",
  "TempPrefMax_estimate",
  "FeedingPath_estimate",
  "to_matrix",
  "Life_span_matrix",
  "Generation_time_matrix",
  "tm_matrix",
  "QB_matrix",
  "BodyShapeI_morphdat",
  "OperculumPresent_morphdat",
  "LLinterrupted_morphdat",
  "Notched_morphdat",
  "DorsalSoftRaysMin_morphdat",
  "DorsalSoftRaysMax_morphdat",
  "FBname_species",
  "AirBreathing_species",
  "PriceCateg_species",
  "Dangerous_species",
  "Electrogenic_species"
)

continuous_traits <- c(
  "a_estimate",
  "b_estimate",
  "ComDepthMax_estimate",
  "ComDepthMin_estimate",
  "DepthMax_estimate",
  "DepthMin_estimate",
  "DorsalSoftRaysMax_morphdat",
  "DorsalSoftRaysMin_morphdat",
  "Generation_time_matrix",
  "K_estimate",
  "Life_span_matrix",
  "MaxLengthSL_estimate",
  "MaxLengthTL_estimate",
  "mean_temp_matrix",
  "PD50_species",
  "PredPreyRatioMax_estimate",
  "PredPreyRatioMin_estimate",
  "QB_matrix",
  "sd_b_estimate",
  "sd_log10a_estimate",
  "seTroph_estimate",
  "TempPrefMax_estimate",
  "TempPrefMean_estimate",
  "TempPrefMin_estimate",
  "tm_matrix",
  "to_matrix",
  "Troph_estimate",
  "Vulnerability_species"
)

discrete_traits <- c(
  "AirBreathing_species",
  "BodyShapeI_morphdat",
  "Brack_species",
  "Dangerous_species",
  "DemersPelag_species",
  "Electrogenic_species",
  "FeedingPath_estimate",
  "Fresh_species",
  "LLinterrupted_morphdat",
  "Notched_morphdat",
  "OperculumPresent_morphdat",
  "Resilience_matrix",
  #"Genus_species",
  "EnvTemp_stocks"
)

meta <- c(
  "FBname_species",
  "spec_code",
  "Species",
  "stock_code"
)


#DISCRETIZATION DICTIONARIES

TROPH_BREAKS <- c(
  -Inf,
  3,
  3.5,
  4,
  Inf
)

TROPH_LABELS <- c(
  "Low",
  "Medium",
  "High",
  "Veryhigh"
)


COM_DEPTH_MAX_BREAKS <- c(
  -Inf,
  20.1,
  54.6,
  148.4,
  Inf
)

COM_DEPTH_MAX_LABELS <- c(
  "Reef",
  "Shallow",
  "Ocean",
  "Deep"
)


DEPTH_MAX_BREAKS <- c(
  -Inf,
  20.1,
  54.6,
  148.4,
  403.4,
  Inf
)

DEPTH_MAX_LABELS <- c(
  "Reef",
  "Shallow",
  "Ocean",
  "Deep",
  "Bathy"
)


MAX_LENGTH_TL_BREAKS <- c(
  -Inf,
  20.1,
  54.6,
  148.4,
  Inf
)

MAX_LENGTH_TL_LABELS <- c(
  "Small",
  "Medium",
  "Large",
  "Very large"
)

AGE_MATURITY_BREAKS <- c( #new #optional: https://www.freshwaterecology.info/fwe_info.php?p=b2c9ZmlzaCNwYXJhbT01Mg==&lang=2&lang=1
  -Inf,
  2.16,
  5.2,
  8,
  Inf
)

AGE_MATURITY_LABELS <- c(
  "Veryearly",
  "Early",
  "Late",
  "Verylate"
)

LIFE_SPAN_BREAKS <- c( #https://www.freshwaterecology.info/fwe_info.php?p=b2c9ZmlzaCNwYXJhbT01MQ==
  -Inf,
  8,
  15,
  Inf
)

LIFE_SPAN_LABELS <- c(
  "ls1",
  "ls2",
  "ls3"
)

#DISCRETIZATION DICTIONARY

DISCRETIZATION_RULES <- list(
  
  Troph_estimate = list(
    breaks = TROPH_BREAKS,
    labels = TROPH_LABELS
  ),
  
  ComDepthMax_estimate = list(
    breaks = COM_DEPTH_MAX_BREAKS,
    labels = COM_DEPTH_MAX_LABELS
  ),
  
  DepthMax_estimate = list(
    breaks = DEPTH_MAX_BREAKS,
    labels = DEPTH_MAX_LABELS
  ),
  
  MaxLengthTL_estimate = list(
    breaks = MAX_LENGTH_TL_BREAKS,
    labels = MAX_LENGTH_TL_LABELS
  ),
  
  tm_matrix = list(
    breaks = AGE_MATURITY_BREAKS,
    labels = AGE_MATURITY_LABELS
  ),
  
  Life_span_matrix = list(
    breaks = LIFE_SPAN_BREAKS,
    labels = LIFE_SPAN_LABELS
  )
  
)

UNUSED_TRAIT_COLUMNS <- c(
  'seTroph_estimate',
  'a_estimate',
  'sd_log10a_estimate',
  'b_estimate',
  'sd_b_estimate',
  'K_estimate',
  'ComDepthMin_estimate',
  'DepthMin_estimate',
  'TempPrefMean_estimate',
  'MaxLengthSL_estimate',
  'mean_temp_matrix',
  'E_matrix',
  "Saltwater_species",
  "LLinterrupted_morphdat"
)

REMAINDER_CONTINUOUS_TRAITS <- c(
  #'PredPreyRatioMin_estimate', not using for the time being because it may not be comparable globally if discretized
  #'PredPreyRatioMax_estimate', not using for the time being because it may not be comparable globally if discretized
  'TempPrefMax_estimate',
  'TempPrefMin_estimate',
  'to_matrix',
  'Life_span_matrix',
#  'Generation_time_matrix',
  'tm_matrix'
#  'QB_matrix',
#  'DorsalSoftRaysMin_morphdat',
#  'DorsalSoftRaysMax_morphdat',
#  'Vulnerability_Species', #use later against probability mapping
#  'PD50_Species' #use later against probability mapping
)
