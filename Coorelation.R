############### pearson coefficient coorelation ########
##########librarries##########
library(vegan)
library(ecodist)
#changing trait matrix into long format one
trait.long<-read.csv(here("..","..","data_files", "Trait similarity matrix", "Euclidean_distance_2400m.csv"))
scm<-read.csv(here("..","..","data_files", "Species co_occurence matrix", "species_species_matrix_2400m.csv"))
# Remove the species name column if present
trait_mat <- as.matrix(trait.long[ , -1])
rownames(trait_mat) <- trait.long$X

# Log-transform to reduce dominance of very large counts
#trait_log <- log1p(trait_mat)
trait_log <- trait_mat
########### Convert trait to a distance matrix############
trait_dist <- as.dist(max(trait_log) - trait_log)

################ sp-co matrices #############
# Remove the species name column if present
scm_mat <- as.matrix(scm[ , -1])
rownames(scm_mat) <- scm$X

# Log-transform to reduce dominance of very large counts
scm_log <- scm_mat
#scm_log <- log1p(scm_mat)
########### Convert co-occurrence to a distance matrix############
scm_dist <- as.dist(max(scm_log) - scm_log)

########### reordering matrix #############
# Species order from co-occurrence distance matrix
sp_order <- attr(scm_dist, "Labels")

# Convert trait distance to matrix
trait_mat <- as.matrix(trait_dist)

# Reorder rows and columns
trait_mat <- trait_mat[sp_order, sp_order]

# Convert back to dist object
trait_dist_fixed <- as.dist(trait_mat)
#######verifying #######
all(
  attr(scm_dist, "Labels") ==
    attr(trait_dist_fixed, "Labels")
)

########## Mantel test #######
mantel_result <- mantel(
  xdis = scm_dist,
  ydis = trait_dist_fixed,
  method = "pearson",
  permutations = 9999
  # The number of times to randomly shuffle the data to calculate the p-value.
  # Since distance data points aren't independent, we can't use standard p-value math.
  # shuffle the matrices 9,999 times to see if the observed correlation is random or real.
)
mantel_result

#plot
# Convert the distance matrices into simple lists (vectors) of numbers
x_vals <- as.vector(as.dist(trait_dist_fixed))
y_vals <- as.vector(as.dist(scm_dist))


########## ecodist ###########
 mantel_result <- mantel(
       formula = scm_dist ~ trait_dist_fixed,
       nperm = 9999,
       mrank = FALSE,  # FALSE = Pearson (Raw values). Change to TRUE for Spearman (Ranks).
       nboot = 500     # ecodist also calculates Confidence Intervals automatically
   )
 
   mantel_result











# Plot
plot(x = x_vals, 
     y = y_vals,
     xlab = "Trait Distance (Dissimilarity)",
     ylab = "Species Community Distance (Dissimilarity)",
     main = "Relationship between Traits and Species Composition",
     pch = 16,      # Solid dots
     col = "blue",  # Color of dots
     cex = 0.5)     # Make dots smaller (easier to see patterns)

# Add a trend line (Linear Regression)
abline(lm(y_vals ~ x_vals), col = "red", lwd = 2)



# 1. Run the mantel test (storing it in 'simple.results.mantel')
 simple.results.mantel <- mantel(
       xdis = scm_dist, 
       ydis = trait_dist_fixed, 
       method = "pearson", 
       permutations = 9999
   )
 
   # 2. Create the Histogram
   # This plots the 9999 random permutations as grey bars
   hist(simple.results.mantel$perm, 
               breaks = 50, 
               main = "Histogram of Random Permutations", 
               xlab = "Correlation (r)", 
               col = "lightgrey",
               xlim = c(-0.4, 0.4)) # Adjusts x-axis to fit your data
 
   # 3. Add a Red Line for YOUR Result
   # This draws a vertical line at -0.2573
   abline(v = simple.results.mantel$statistic, 
                   col = "red", 
                   lwd = 3, 
                   lty = 2)
 
   # 4. Add a text label to point out your result
   text(x = simple.results.mantel$statistic, 
               y = 0, 
               labels = "Your Result\n(-0.257)", 
               col = "red", 
               pos = 3, # Position text above the line
               cex = 0.8)
 #plot
   # Convert the distance matrices into simple lists (vectors) of numbers
   x_vals <- as.vector(as.dist(trait_dist_fixed))
 y_vals <- as.vector(as.dist(scm_dist))
 

