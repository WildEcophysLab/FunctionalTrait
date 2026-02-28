# Load necessary libraries
library(ecodist)
library(here)

# Reading the Euclidean distance and co-occurrence files
trait_raw <- read.csv(here("..", "..", "data_files", "Trait similarity matrix", "Euclidean_distance_2800m.csv"))
scm_raw   <- read.csv(here("..", "..", "data_files", "Species co_occurence matrix", "species_species_matrix_2800m.csv"))

# Convert to matrix and set row names from the first column (X)
trait_mat <- as.matrix(trait_raw[, -1])
rownames(trait_mat) <- trait_raw$X

# Convert trait similarity to a distance object
trait_dist <- as.dist(max(trait_mat) - trait_mat)

#Processing Co-occurrence Matrix 
scm_mat <- as.matrix(scm_raw[, -1])
rownames(scm_mat) <- scm_raw$X

# Convert co-occurrence to a distance object
scm_dist <- as.dist(max(scm_mat) - scm_mat)

# The Mantel test requires both matrices to have species in the exact same order
sp_order <- labels(scm_dist)

# Reorder the trait matrix to match the co-occurrence order
trait_mat_ordered <- as.matrix(trait_dist)[sp_order, sp_order]
trait_dist_final  <- as.dist(trait_mat_ordered)

# Validation & Analysis ---
# Verify if the labels now match perfectly
if(all(labels(scm_dist) == labels(trait_dist_final))) {
  message("Success: Matrices are aligned. Running Mantel Test...")
  
  # Performing the Mantel Test
  mantel_result <- ecodist::mantel(
    scm_dist ~ trait_dist_final,
    nperm = 9999,  # nperm: Number of permutations for p-value calculation
    mrank = FALSE, 
    nboot = 500    # nboot: Number of iterations for the 95% confidence interval
  )
  
  print(mantel_result)
} else {
  stop("Error: Species labels do not match between matrices.")
}