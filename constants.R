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