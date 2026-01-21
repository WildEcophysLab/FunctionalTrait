{################ elevation code with attributes #################
data800m <- splited_fc[["800"]]  

# remove flocks with just one species + add species count per flock
Splevel_data <- data800m %>%
  group_by(flock_id) %>%
  mutate(n_species = length(unique(species))) %>%
  ungroup() %>%
  filter(n_species > 1)

# unique species and flocks
sp800 <-  sort(unique(Splevel_data$species))
fl800 <- sort(unique(Splevel_data$flock_id))

# ---------------------------
# flock-by-species matrix 800m
# ---------------------------
obs_mat.800 <- matrix(0, nrow = length(fl800), ncol = length(sp800))
dimnames(obs_mat.800) <- list(fl800, sp800)

for(i in 1:length(fl800)){
  # 🔹 CHANGE: use Splevel_data instead of fc
  temp <- subset(Splevel_data, flock_id == fl800[i])  
  
  # 🔹 CHANGE: put abundance instead of just 1
  obs_mat.800[i, temp$species] <- temp$abundance  
}

# ---------------------------
# species-by-species matrix
# ---------------------------
obs.mat.adj.800 <- matrix(0, length(sp800), length(sp800))
dimnames(obs.mat.adj.800) <- list(sp800, sp800)

for(m in 1:length(sp800)){
  for(n in 1:length(sp800)){
    # 🔹 CHANGE: use abundance co-occurrence instead of just presence
    obs.mat.adj.800[m,n] <- sum(
      obs_mat.800[, sp800[m]] > 0 & obs_mat.800[, sp800[n]] > 0
    )
  }
}

# make igraph
diag(obs.mat.adj.800) <- 0
obs.net.igraph.800 <- graph.adjacency(obs.mat.adj.800, mode = "undirected", weighted = TRUE)

# ---------------------------
# add species attributes
# ---------------------------
# 🔹 aggregate abundance across flocks for each species
sp_abundance <- Splevel_data %>%
  group_by(species) %>%
  summarise(total_abundance = sum(abundance))

# 🔹 assign as vertex attribute
V(obs.net.igraph.800)$abundance <- sp_abundance$total_abundance[match(V(obs.net.igraph.800)$name, sp_abundance$species)]
}
# Flock id 367 does not having single individual value 

################# PCA by myself, calculating ecludian distance###############
library(here)
library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
#######Reading files here##############
fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
#morpho.d<-read.csv(here("..","..","data_files", "avo_clean.csv"))
#morpho.mean<-read.csv(here("..","..","data_files","avo_sp_mean.csv")) # this has trait mean and SD for each trait of species
suply<-read.csv(here("..","..","data_files","Supplementary_cleaned.csv"))

################# remove SD columns from Morpho.mean###############
#morpho.mean <- morpho.mean %>% 
#      select(-contains("sd"))

############# Taken out only trait data which will be used in futher analysis#####################
#trait.data<-morpho.d %>%
 # select(species,Beak.Depth,Beak.Length_Culmen,Beak.Length_Nares,Beak.Width,Tarsus.Length,Kipps.Distance,Wing.Length,Secondary1,Tail.Length,Hand.wing.Index)

#Species<- morpho.mean$species
#Trait<-morpho.mean[,2:12]


#################### PCA pipeline for all species in Mixed flocking species #################
Species<- suply$species
Trait<-suply[,c(10:20,1)]

Trait.PCA<- princomp(Trait[,1:11], cor=FALSE)
Trait.PCA2<- prcomp(Trait[,1:11], scale. = FALSE)

biplot(Trait.PCA2)
biplot(Trait.PCA)

#For calclating ecludian distance taking out the scores
Eucd<-Trait.PCA2$x[,1:2]  
Eucd<-as.data.frame(Eucd)

#adding species name in dataframe
Eucd$Species<-Trait$species


#calculating ecludian distance and computing Euclidean distance matrix
dist_matrix <- as.matrix(dist(Eucd[, c("PC1", "PC2")], method = "euclidean"))
rownames(dist_matrix) <- Eucd$Species
colnames(dist_matrix) <- Eucd$Species


#normal PCA Plot
ggplot(Trait.PCA2$x, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "PC1", y = "PC2")



################ spliting trait data elevation wise ###################
# 1. Create elevation band
fc$elev_band <- as.factor(fc$elevation)

# 2. Create species × elevation mapping
species_by_elev <- fc %>%
  distinct(species, elev_band)

# 3. Create elevation-wise trait datasets
suply_elev_list <- list()   # empty list

elevations <- unique(species_by_elev$elev_band)

 for (e in elevations) {
  
  # species at this elevation
  sp_e <- species_by_elev$species[species_by_elev$elev_band == e]
  
  # subset trait data
  suply_elev_list[[as.character(e)]] <- 
    suply[suply$species %in% sp_e, ]
  
 }

#################### Trait coloumn ###################
trait_cols <- c(
  "Beak.Length_Culmen",
  "Beak.Length_Nares",
  "Beak.Width",
  "Beak.Depth",
  "Tarsus.Length",
  "Wing.Length",
  "Kipps.Distance",
  "Secondary1",
  "Hand.Wing.Index",
  "Tail.Length",
  "Mass"
)

#################### PCA pipeline for elevation bands ############
# creating elmpty lists
PCA_list      <- list()
Eucd_list     <- list()
distmat_list  <- list()
#Loop over elavtions
for (e in names(suply_elev_list)) {
  
  df <- suply_elev_list[[e]]   # trait data for this elevation
  Species <- df$species
  # keep ONLY chosen morphological traits
  Trait <- df[, trait_cols]
  
  # run PCA
  Trait.PCA2 <- prcomp(Trait, scale. = FALSE)
  
  # extract PC1 & PC2 scores
  Eucd <- as.data.frame(Trait.PCA2$x[, 1:2])
  Eucd$Species <- df$species
  
  # Euclidean distance matrix
  dist_matrix <- as.matrix(
    dist(Eucd[, c("PC1", "PC2")], method = "euclidean")
  )
  
  rownames(dist_matrix) <- Eucd$Species
  colnames(dist_matrix) <- Eucd$Species
  
  # store outputs
  PCA_list[[e]]     <- Trait.PCA2
  Eucd_list[[e]]    <- Eucd
  distmat_list[[e]] <- dist_matrix
}

####### Saving matrix as csv ##############
for (e in names(distmat_list)) {
  
  write.csv(
    distmat_list[[e]],
    file = paste0("Euclidean_distance_PC_", e, "m.csv"),
    row.names = TRUE
  )
}
############# extra ################
am800m<-read.csv(here("..","..","data_files", "Edge_list_Adjacency_list", "species_species_matrix_800m.csv"))


m.800<-melt(am800m)

ggplot(am800m, aes(x=value))+geom_histogram()






tm2800<-read.csv(here("..","..","data_files", "Trait similarity matrix", "Euclidean_distance_2800m.csv"))


######### 10-01-2026##############
#reading ecludian  matrix for coorelation
em2400<-read.csv(here("..","..","data_files", "Trait similarity matrix", "Euclidean_distance_2400m.csv"))
#reading sp-cooccurence matrix
scm2800<-read.csv(here("..","..","data_files", "Species co_occurence matrix", "species_species_matrix_2800m.csv"))

