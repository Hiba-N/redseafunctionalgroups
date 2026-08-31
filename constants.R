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
  "Reproductive_guild_matrix"
)

# Values to treat as missing
MISSING_VALUES <- c("", "unknown", "NA")