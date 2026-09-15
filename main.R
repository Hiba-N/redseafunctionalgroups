library(rfishbase)
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
source("utils/utils.R")
source("constants/constants.R")
source("constants/model_constants.R")
library(readr)
library(writexl)
source("utils/model_utils.R")
library(PCAmixdata)
library(nomclust)
library(mclust)




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

#deleting unnecessary columns (such as metadata) except for reference columns
red_sea_final <- remove_columns(
  red_sea_final,
  META_COLUMNS_TO_REMOVE
)

# Calculate missing percentage for all columns
missing_data <- calculate_missing_percentage(red_sea_final)

trait_table <- create_trait_table(
  red_sea_final,
  selected_traits
)

missing_data <- calculate_missing_percentage(trait_table)

sapply(trait_table[continuous_traits], class)

plot_continuous_distributions(
  data = trait_table,
  columns = continuous_traits
)

sapply(trait_table[discrete_traits], class)

plot_discrete_distributions(
  data = trait_table,
  columns = discrete_traits
)

#fixing a few random values
trait_table <- trait_table %>%
  mutate(
    Resilience_matrix = ifelse(
      tolower(trimws(Resilience_matrix)) == "please enter values for k, tmax.",
      NA,
      Resilience_matrix
    )
  )

trait_table[discrete_traits] <- lapply(
  trait_table[discrete_traits],
  as.factor
)

trait_table$BodyShapeI_morphdat[
  trait_table$BodyShapeI_morphdat == "other (see remarks)"
] <- "other"

trait_table$Electrogenic_species[
  trait_table$Electrogenic_species == "Electrosensing only"
] <- "electrosensing only"

table(
  trait_table$BodyShapeI_morphdat,
  useNA = "ifany"
)

#KNN

results <- run_knn_experiments(
  
  data =
    trait_table,
  
  exclude_columns =
    EXCLUDED_COLUMNS,
  
  k_values =
    K_VALUES,
  
  weighted_values =
    WEIGHTED_VALUES,
  
  mask_proportions =
    MASK_PROPORTIONS,
  
  seeds =
    SEEDS
)


write.csv(
  results,
  "results/9-3-2026/gower_results.csv",
  row.names = FALSE
)


results_summary <- summarise_knn_results(
  results
)


write.csv(
  results_summary,
  "results/9-3-2026/gower_results_summary.csv",
  row.names = FALSE
)


best_parameters <- get_best_parameters(
  results
)

write.csv(
  best_parameters,
  "results/9-3-2026/knn_gower_best_parameters.csv",
  row.names = FALSE
)


traits_imputed <- impute_using_best_parameters(
  data = trait_table,
  best_parameters = best_parameters,
  exclude_columns = EXCLUDED_COLUMNS
)

missing_data <- calculate_missing_percentage(traits_imputed)

#trying for f1 weighted instead of micro  #do this later

#pca, before and after
pca_original_results <- run_pcamix_analysis(
  data = trait_table,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  ndim = 5,
  graph = FALSE
)

pca_original_results$correlation_plot 
view(pca_original_results$discrete_table) 
names(pca_original_results$pcamix) 
head(pca_original_results$pcamix$ind$coord) 
names(pca_original_results$pcamix) 
str(pca_original_results$pcamix, max.level = 2) 
summary(pca_original_results$pcamix) 
head(pca_original_results$pcamix$ind$coord) 
eig <- pca_original_results$pcamix$eig 
eig_df <- data.frame( dimension = seq_len(nrow(eig)), eigenvalue = eig[, "Eigenvalue"], percentage = eig[, "Proportion"], cumulative = eig[, "Cumulative"] ) 
ggplot(eig_df[1:20, ], aes(dimension, eigenvalue)) + geom_point() + geom_line() + labs( x = "Dimension", y = "Eigenvalue", title = "PCA-Mix Scree Plot" ) + theme_minimal()

#pca, before and after
pca_inferred_results <- run_pcamix_analysis(
  data = traits_imputed,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  ndim = 5,
  graph = TRUE
)

pca_inferred_results$correlation_plot 
view(pca_inferred_results$discrete_table) 
names(pca_inferred_results$pcamix) 
head(pca_inferred_results$pcamix$ind$coord) 
names(pca_inferred_results$pcamix) 
str(pca_inferred_results$pcamix, max.level = 2) 
summary(pca_inferred_results$pcamix) 
head(pca_inferred_results$pcamix$ind$coord) 
eig <- pca_inferred_results$pcamix$eig 
eig_df <- data.frame( dimension = seq_len(nrow(eig)), eigenvalue = eig[, "Eigenvalue"], percentage = eig[, "Proportion"], cumulative = eig[, "Cumulative"] ) 
ggplot(eig_df[1:20, ], aes(dimension, eigenvalue)) + geom_point() + geom_line() + labs( x = "Dimension", y = "Eigenvalue", title = "PCA-Mix Scree Plot" ) + theme_minimal()


#comparing inferred data with base models (averages and modes)

baseline_results <- run_baseline_models(
  data = trait_table,
  continuous_traits = continuous_traits,
  discrete_traits = discrete_traits,
  mask_proportion = 0.10,
  seed = 123
)

write.csv(
  baseline_results ,
  "results/9-6-2026/baseline_results.csv",
  row.names = FALSE
)

#manual comparison done, consider doing a technical one | knn works better overall

#continuous to discrete

discrete_class_counts <- get_class_counts(
  data = traits_imputed,
  discrete_traits
)

View(discrete_class_counts)

#apart from binary flags we usually have classes 4,5,7,8 so we should go for around 6 classes

#plotting histograms
plot_continuous_distributions(
  data = traits_imputed,
  columns = continuous_traits
)


#discretizing according to literature
traits_discrete <- discretize_traits(traits_imputed)

#deleting unwanted columns
traits_discrete <- remove_columns(
  traits_half_discrete,
  FINAL_REMOVED
)


traits_discrete_only <- traits_discrete %>%
  select(where(is.factor))



#distance

# Goodall 3
distance_goodall <- goodall3(traits_discrete_only)

# Lin
distance_lin <- lin(traits_discrete_only)

# Eskin
distance_eskin <- eskin(traits_discrete_only)


head(as.matrix(distance_goodall))
head(as.matrix(distance_lin))
head(as.matrix(distance_eskin))

distance_matrices <- list(
  Goodall = distance_goodall,
  Lin = distance_lin,
  Eskin = distance_eskin
)

write.csv(
  as.matrix(distance_goodall),
  "results/distance_goodall.csv"
)

write.csv(
  as.matrix(distance_lin),
  "results/distance_lin.csv"
)

write.csv(
  as.matrix(distance_eskin),
  "results/distance_eskin.csv"
)

summary(distance_goodall)
summary(distance_lin)
summary(distance_eskin)

range(distance_goodall)
range(distance_lin)
range(distance_eskin)

hc_goodall <- hclust(distance_goodall, method = "average")
hc_lin <- hclust(distance_lin, method = "average")
hc_eskin <- hclust(distance_eskin, method = "average")

plot(
  hc_goodall,
  main = "Hierarchical clustering - Goodall",
  xlab = "",
  sub = ""
)

plot(
  hc_lin,
  main = "Hierarchical clustering - Lin",
  xlab = "",
  sub = ""
)

plot(
  hc_eskin,
  main = "Hierarchical clustering - Eskin",
  xlab = "",
  sub = ""
)

cluster_range <- 2:25

silhouette_results <- data.frame(
  k = cluster_range,
  Goodall = NA_real_,
  Lin = NA_real_,
  Eskin = NA_real_
)

for (i in seq_along(cluster_range)) {
  
  k <- cluster_range[i]
  
  # Goodall
  groups_goodall <- cutree(hc_goodall, k = k)
  sil_goodall <- silhouette(groups_goodall, distance_goodall)
  silhouette_results$Goodall[i] <- mean(sil_goodall[, "sil_width"])
  
  # Lin
  groups_lin <- cutree(hc_lin, k = k)
  sil_lin <- silhouette(groups_lin, distance_lin)
  silhouette_results$Lin[i] <- mean(sil_lin[, "sil_width"])
  
  # Eskin
  groups_eskin <- cutree(hc_eskin, k = k)
  sil_eskin <- silhouette(groups_eskin, distance_eskin)
  silhouette_results$Eskin[i] <- mean(sil_eskin[, "sil_width"])
}

silhouette_results

write.csv(
  silhouette_results,
  "results/silhouette_scores.csv",
  row.names = FALSE
)

matplot(
  silhouette_results$k,
  silhouette_results[, -1],
  type = "b",
  pch = 19,
  lty = 1,
  xlab = "Number of clusters (k)",
  ylab = "Mean silhouette width",
  main = "Silhouette scores by distance metric"
)

legend(
  "topright",
  legend = c("Goodall", "Lin", "Eskin"),
  lty = 1,
  pch = 19
)

# ============================================================
# ADJUSTED RAND INDEX
# ============================================================

library(mclust)

ari_results <- data.frame(
  k = cluster_range,
  Goodall_Lin = NA_real_,
  Goodall_Eskin = NA_real_,
  Lin_Eskin = NA_real_
)

for (i in seq_along(cluster_range)) {
  
  k <- cluster_range[i]
  
  # Get clusters
  groups_goodall <- cutree(hc_goodall, k = k)
  groups_lin <- cutree(hc_lin, k = k)
  groups_eskin <- cutree(hc_eskin, k = k)
  
  # ARI comparisons
  ari_results$Goodall_Lin[i] <- adjustedRandIndex(
    groups_goodall,
    groups_lin
  )
  
  ari_results$Goodall_Eskin[i] <- adjustedRandIndex(
    groups_goodall,
    groups_eskin
  )
  
  ari_results$Lin_Eskin[i] <- adjustedRandIndex(
    groups_lin,
    groups_eskin
  )
}

ari_results

write.csv(
  ari_results,
  "results/ari_results.csv",
  row.names = FALSE
)

matplot(
  ari_results$k,
  ari_results[, -1],
  type = "b",
  pch = 19,
  lty = 1,
  xlab = "Number of clusters (k)",
  ylab = "Adjusted Rand Index",
  main = "Agreement between distance metrics"
)

legend(
  "topright",
  legend = c(
    "Goodall vs Lin",
    "Goodall vs Eskin",
    "Lin vs Eskin"
  ),
  lty = 1,
  pch = 19
)

best_silhouette <- data.frame(
  Distance = c("Goodall", "Lin", "Eskin"),
  Best_k = c(
    silhouette_results$k[
      which.max(silhouette_results$Goodall)
    ],
    silhouette_results$k[
      which.max(silhouette_results$Lin)
    ],
    silhouette_results$k[
      which.max(silhouette_results$Eskin)
    ]
  ),
  Max_silhouette = c(
    max(silhouette_results$Goodall),
    max(silhouette_results$Lin),
    max(silhouette_results$Eskin)
  )
)

best_silhouette


groups_eskin_2 <- cutree(hc_eskin, k = 2)
traits_with_groups <- traits_discrete_only
traits_with_groups$Group <- factor(groups_eskin_2)
group_trait_summary <- lapply(
  traits_discrete_only,
  function(x) {
    prop.table(table(x, traits_with_groups$Group), margin = 2)
  }
)
group_trait_summary




# Add cluster assignments
traits_with_groups <- traits_discrete_only
traits_with_groups$Group <- factor(groups_eskin_2)

# Calculate percentages for every trait
percentage_results <- do.call(
  rbind,
  lapply(names(traits_discrete_only), function(trait) {
    
    tab <- prop.table(
      table(
        traits_discrete_only[[trait]],
        traits_with_groups$Group
      ),
      margin = 2
    ) * 100
    
    result <- as.data.frame(tab)
    
    colnames(result) <- c(
      "Category",
      "Group",
      "Percentage"
    )
    
    result$Trait <- trait
    
    result[, c(
      "Trait",
      "Category",
      "Group",
      "Percentage"
    )]
  })
)

# Save
write.csv(
  percentage_results,
  "results/eskin_cluster_trait_percentages.csv",
  row.names = FALSE
)