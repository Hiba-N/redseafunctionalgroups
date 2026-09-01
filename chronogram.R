install.packages(c(
  "fishtree",
  "ape"
))

install.packages(
  "Rphylopars"
)

library(fishtree)
library(ape)
library(dplyr)
library(stringr)
library(Rphylopars)

head(red_sea_final$Species)

red_sea_phylo_data <- red_sea_final %>%
  mutate(
    phylo_species = str_replace_all(
      Species,
      " ",
      "_"
    )
  )

fish_species <- unique(red_sea_phylo_data$Species)
View(fish_species)

fish_tree <- fishtree_phylogeny(
  species = fish_species,
  type = "chronogram"
)

fish_tree

plot(fish_tree, cex = 0.3)