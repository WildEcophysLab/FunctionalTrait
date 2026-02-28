################# PCA by myself, calculating ecludian distance###############
library(here)
library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
#######Reading files here##############
fc<-read.csv(here("..","..","data_files","Extra", "flock_composition_data_cleaned.csv"))

# has all the species and their mean traits values with SD 
mean_com <-read.csv(here("..","..","data_files","ready_data", "avonet_mean_combined.csv"))
#Extract only mean values for PCA (removing SD columns)
mean_com <- mean_com %>%
  select(common_name, ebird_sci_resolved, 
         ends_with("_mean"), n_samples) %>%
  rename_with(~gsub("_mean$", "", .), ends_with("_mean"))
# OR
mean_com <- mean_com %>% 
     select(-contains("sd"))


## spliting the trait data here onwards to do have seprate trait 
# 1. Read the co-occurrence matrix
fc800m <- read.csv(here("..", "..", "data_files","Species co_occurence matrix","species_species_matrix_800m.csv"))

# 2. Extract species names from column names (excluding the first column "X")
species.800 <- colnames(fc800m)[-1]

avonet_fc.800m <- mean_com %>%
  filter(common_name %in% species.800)

#################### PCA pipeline for all species in Mixed flocking species #################

Species<- species.800
Trait<-avonet_fc.800m[,c(3:12,1)]


Trait.PCA2<- prcomp(Trait[,1:10], scale. = FALSE)

biplot(Trait.PCA2)

#For calclating ecludian distance taking out the scores
Eucd<-Trait.PCA2$x[,1:2]  
Eucd<-as.data.frame(Eucd)

#adding species name in dataframe
Eucd$Species<-Trait$common_name

#calculating ecludian distance and computing Euclidean distance matrix
dist_matrix.800 <- as.matrix(dist(Eucd[, c("PC1", "PC2")], method = "euclidean"))
rownames(dist_matrix.800) <- Eucd$Species
colnames(dist_matrix.800) <- Eucd$Species

#saving the files
write.csv(dist_matrix.800, here::here("..","..","data_files","ready_data", "eud_dist_800m.csv"), row.names = FALSE)

#normal PCA Plot
ggplot(Trait.PCA2$x, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "PC1", y = "PC2")