# Indicator species analysis
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(here)
library(vegan)
library(indicspecies)

#msf has columns FlockID, Elevation, Strata, Species, Abundance


#abundance
msf %>%
  filter(is.na(min_ind) & is.na(max_ind)) %>%
  select(flock_id, species, min_ind, max_ind)

#removing all NAs flocks
msf.1<-msf %>% drop_na(abundance)

# Suppose `flock_df` has columns FlockID, Elevation, Strata, Species, Abundance.
comm_df <- msf.1%>%
  pivot_wider(
    id_cols = c(flock_id, elevation, strata),
    names_from = species,
    values_from = abundance,
    values_fill = 0,
    values_fn = sum
  )

# to check To Check Duplicate species in flocks
msf %>%
  count(flock_id, species) %>%
  filter(n > 1)


# Extract community matrix and group factors
comm <- as.matrix(comm_df %>% select(-flock_id, -elevation, -strata))
group_elev <- factor(comm_df$elevation)  # e.g. "Low","Mid","High"
group_strata <- factor(comm_df$strata)   # e.g. "Understory","Canopy"

#ISA
result_strata<- multipatt(comm, group_strata, func="IndVal.g", control=how(nperm=999))

summary(result)


# removes all messy combinations.
result_strata<<- multipatt(comm, group_strata,
                    func="IndVal.g",
                    duleg = TRUE, # important
                    control=how(nperm=999))



# combined strata and elevation 
comm_df_combined <- comm_df %>%
  mutate(group_comb = paste(elevation, strata, sep = "_"))


comm_combined <- comm_df_combined %>%
  select(-flock_id, -elevation, -strata, -group_comb) %>%
  as.matrix()

group_comb <- factor(comm_df_combined$group_comb)

table(group_comb)


library(indicspecies)

result_comb <- multipatt(
  comm_combined,
  group_comb,
  func = "IndVal.g",
  duleg = TRUE,
  control = how(nperm = 999)
)

summary(result_comb)




#If you want only single groups:
  
result_comb_single <- multipatt(
    comm,
    group_comb,
    func = "IndVal.g",
    duleg = TRUE,   # only single groups
    control = how(nperm = 999)
  )






#ploting result comb only significant species 

ind_table <- as.data.frame(result_comb_single$sign)
ind_table$species <- rownames(ind_table)

# Keep only significant species
sig_ind <- ind_table %>%
  filter(p.value < 0.05)

#converting that into one “Elevation” column.
sig_long <- sig_ind %>%
  pivot_longer(cols = starts_with("s."),
               names_to = "Elevation",
               values_to = "Indicator") %>%
  filter(Indicator == 1)

# Clean elevation names
sig_long$Elevation <- gsub("s.", "", sig_long$Elevation)




# Extracting Strong Indicators

library(dplyr)
library(tidyr)

# Convert ISA output to dataframe
ind_table <- as.data.frame(result_comb_single$sign)
ind_table$species <- rownames(ind_table)

# Keep strong indicators only
strong_ind <- ind_table %>%
  filter(stat > 0.4, p.value < 0.05)


#STEP 2 — Convert Group Columns to One Column
strong_long <- strong_ind %>%
  pivot_longer(cols = starts_with("s."),
               names_to = "Group",
               values_to = "Indicator") %>%
  filter(Indicator == 1)

# Clean group names
strong_long$Group <- gsub("s.", "", strong_long$Group)
#plot
library(ggplot2)

ggplot(strong_long,
       aes(x = stat,
           y = reorder(species, stat),
           color = Group)) +
  geom_point(size = 4) +
  labs(x = "Indicator Value (IndVal)",
       y = "Species",
       title = "Strong Indicator Species (IndVal > 0.4)",
       color = "Elevation_Strata") +
  theme_minimal(base_size = 14)








################# contour/isoline plot ####################
# R code to recreate the A vs B contour plots and overlay species
# (Replace comm and group_elev with your data)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) compute A (specificity) and B (fidelity) for each species and group ----
# comm: matrix (sites x species), group_elev: factor (site-level grouping)

# helper: mean abundance of species i in group j
mean_in_group <- function(mat, groups) {
  # mat: sites x species
  by_group_means <- t(sapply(levels(groups), function(g) {
    rows <- which(groups == g)
    if(length(rows)==0) return(rep(NA, ncol(mat)))
    colMeans(mat[rows, , drop=FALSE])
  }))
  # matrix rows = groups, cols = species
  rownames(by_group_means) <- levels(groups)
  by_group_means
}

# compute group means and frequencies
group_means <- mean_in_group(comm, group_elev)  # groups x species
group_freqs <- t(sapply(levels(group_elev), function(g) {
  rows <- which(group_elev == g)
  if(length(rows)==0) return(rep(NA, ncol(comm)))
  colSums(comm[rows, , drop=FALSE] > 0) / length(rows)  # proportion of sites with presence
}))
rownames(group_freqs) <- levels(group_elev)

# for each species, compute A and B for each group:
# A_ij = mean_abundance_in_group_j / sum(mean_abundance_across_groups)
# B_ij = frequency_in_group_j (proportion of sites in group with species)
A <- apply(group_means, 2, function(x) x / sum(x, na.rm=TRUE))  # groups x species
B <- group_freqs  # groups x species (already proportions)

# Convert to a long table with one row per species-group
species_groups <- expand.grid(
  group = rownames(A),
  species = colnames(A),
  stringsAsFactors = FALSE
)
species_groups <- species_groups %>%
  mutate(A = as.vector(A),      # specificity
         B = as.vector(B),
         IndVal = A * B,
         sqrtInd = sqrt(IndVal))

# ---- 2) For plotting, take the group each species is best associated with (max IndVal) ----
best_group <- species_groups %>%
  group_by(species) %>%
  slice_max(order_by = IndVal, n = 1, with_ties = FALSE) %>%
  ungroup()

# If you want only species that were included/selectable, filter as needed:
# e.g., keep species that appear in result$sign (significant or selected)
# Suppose `result$sign` exists, you can get significant species as:
# sig_sp <- rownames(result$sign)[result$sign$p.value < 0.05]
# best_group <- filter(best_group, species %in% sig_sp)

# ---- 3) create underlying contour grid for IndVal and sqrt(IndVal) ----
grid <- expand.grid(A = seq(0.001, 0.999, length.out = 200),
                    B = seq(0.001, 0.999, length.out = 200))
grid$IndVal <- grid$A * grid$B
grid$sqrtInd <- sqrt(grid$IndVal)

# prepare contour levels (choose breaks you like)
cont_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.9)

# ---- 4) plot IndVal contours and overlay species points (colored by best group) ----
p1 <- ggplot(grid, aes(x = A, y = B, z = IndVal)) +
  geom_tile(aes(fill = IndVal)) +
  geom_contour(breaks = cont_breaks, colour = "black", size = 0.3) +
  scale_fill_viridis_c(name = "IndVal", option = "viridis", direction = 1) +
  geom_point(data = best_group, aes(x = A, y = B, color = group), size = 2, alpha = 0.9) +
  geom_text(data = best_group %>% filter(IndVal > 0.1), # label only reasonably strong ones
            aes(x = A, y = B, label = species), hjust = -0.05, vjust = 0.5, size = 3) +
  labs(x = "Specificity (Aij)", y = "Fidelity (Bij)",
       title = "Indicator Value (IndVal = A × B) — species overlaid") +
  theme_minimal()

# sqrt transform panel
p2 <- ggplot(grid, aes(x = A, y = B, z = sqrtInd)) +
  geom_tile(aes(fill = sqrtInd)) +
  geom_contour(breaks = sqrt(cont_breaks), colour = "black", size = 0.3) +
  scale_fill_viridis_c(name = "sqrt(IndVal)", option = "viridis", direction = 1) +
  geom_point(data = best_group, aes(x = A, y = B, color = group), size = 2, alpha = 0.9) +
  geom_text(data = best_group %>% filter(IndVal > 0.1),
            aes(x = A, y = B, label = species), hjust = -0.05, vjust = 0.5, size = 3) +
  labs(x = "Specificity (Aij)", y = "Fidelity (Bij)",
       title = "Square root of IndVal — species overlaid") +
  theme_minimal()

# arrange side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)