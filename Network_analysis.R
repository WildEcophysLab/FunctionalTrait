#Network analysis
library(here)
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(visNetwork)

##### reading datafiles  #####
fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))

# without here function
fc<- read.csv("C:/Users/sandy/Downloads/flock_composition_data_cleaned.csv")


##############subseting of fc data only having usable coloumns############
fcibj<- fc %>%
   select(flock_id,elevation,species,canopy_level,strata)

############spliting flock data#############
#by elevation making list
splited_fc<-split(fcibj, fc$elevation)

############ 800m matrix and plot###########
data800m <- splited_fc[["800"]]  
  
#removing flocks with just one species. since they are not mixed and adding total no of species in a flock!
Splevel_data <- data800m %>%
  group_by(flock_id) %>%
  mutate(n_species = length(unique(species))) %>%
  ungroup() %>%
  filter(n_species > 1)

# 800m
sp800 <-  sort(unique(Splevel_data$species))
fl800 <- sort(unique(Splevel_data$flock_id))


# flock-by-species matrix 800m
obs_mat.800 <- matrix(0, nrow = length(fl800), ncol = length(sp800))
dimnames(obs_mat.800) <- list(fl800, sp800)
for(i in 1:length(fl800)){
  temp <- subset(fc, flock_id == fl800[i])
  obs_mat.800[i, which(colnames(obs_mat.800) %in% temp$species)] <- 1
}

# species-by-species matrix
obs.mat.adj.800 <- matrix(0, length(sp800), length(sp800)); #created a empty matrix
dimnames(obs.mat.adj.800) <- list(sp800, sp800) #assign names to the rows and coloumns on new matrix
for(m in 1:length(sp800)){
  for(n in 1:length(sp800))
  {
    obs.mat.adj.800[m,n] <- length(which(obs_mat.800[,paste(sp800[m])] == 1 & obs_mat.800[,paste(sp800[n])] == 1))
  }
}

# g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
# plot(g800, edge.width=E(g800)$weight)


sp.rich.800 <- length(sp800)
sp.rich.800


diag(obs.mat.adj.800) <- 0
obs.net.igraph.800 <- graph.adjacency(obs.mat.adj.800, mode = "undirected", weighted = TRUE)

obs.density.800 <- round(edge_density(obs.net.igraph.800), 2)
obs.density.800

# species properties: weighted degree #
obs.degree.800 <- degree(obs.net.igraph.800)
sort(obs.degree.800, decreasing = T)
hist(obs.degree.800, 10)

############ 1200m matrix and plot  ###########
  data1200m <- splited_fc[["1200"]]  
  
  #removing flocks with just one species. since they are not mixed!
  Splevel_data.1200 <- data1200m %>%
    group_by(flock_id) %>%
    mutate(n_species = length(unique(species))) %>%
    ungroup() %>%
    filter(n_species > 1)
  
  
  
  # 1200m
  sp1200 <-  sort(unique(Splevel_data.1200$species))
  fl1200 <- sort(unique(Splevel_data.1200$flock_id))
  
  
  
  # flock-by-species matrix 1200m
  obs_mat.1200 <- matrix(0, nrow = length(fl1200), ncol = length(sp1200))
  dimnames(obs_mat.1200) <- list(fl1200, sp1200)
  for(i in 1:length(fl1200)){
    temp <- subset(fc, flock_id == fl1200[i])
    obs_mat.1200[i, which(colnames(obs_mat.1200) %in% temp$species)] <- 1
  }
  
  
  # species-by-species matrix
  obs.mat.adj.1200 <- matrix(0, length(sp1200), length(sp1200)); #created a empty matrix
  dimnames(obs.mat.adj.1200) <- list(sp1200, sp1200) #assign names to the rows and coloumns on new matrix
  for(m in 1:length(sp1200)){
    for(n in 1:length(sp1200))
    {
      obs.mat.adj.1200[m,n] <- length(which(obs_mat.1200[,paste(sp1200[m])] == 1 & obs_mat.1200[,paste(sp1200[n])] == 1))
    }
  }
  
  
  
  # g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
  # plot(g800, edge.width=E(g800)$weight)
  
  
  sp.rich.1200 <- length(sp1200)
  sp.rich.1200
  
  
  diag(obs.mat.adj.1200) <- 0
  obs.net.igraph.1200 <- graph.adjacency(obs.mat.adj.1200, mode = "undirected", weighted = TRUE)
  
  obs.density.1200 <- round(edge_density(obs.net.igraph.1200), 2)
  obs.density.1200
  # species properties: weighted degree 
  obs.degree.1200 <- degree(obs.net.igraph.1200)
  sort(obs.degree.1200, decreasing = T)
  hist(obs.degree.1200, 10)
  
  

############ 1600m ###########
  data1600m <- splited_fc[["1600"]]  
  
  #removing flocks with just one species. since they are not mixed!
  Splevel_data.1600 <- data1600m %>%
    group_by(flock_id) %>%
    mutate(n_species = length(unique(species))) %>%
    ungroup() %>%
    filter(n_species > 1)
  
  
  
  # 1600m
  sp1600 <-  sort(unique(Splevel_data.1600$species))
  fl1600 <- sort(unique(Splevel_data.1600$flock_id))
  
  
  
  # flock-by-species matrix 1600m
  obs_mat.1600 <- matrix(0, nrow = length(fl1600), ncol = length(sp1600))
  dimnames(obs_mat.1600) <- list(fl1600, sp1600)
  for(i in 1:length(fl1600)){
    temp <- subset(fc, flock_id == fl1600[i])
    obs_mat.1600[i, which(colnames(obs_mat.1600) %in% temp$species)] <- 1
  }
  
  
  # species-by-species matrix
  obs.mat.adj.1600 <- matrix(0, length(sp1600), length(sp1600)); #created a empty matrix
  dimnames(obs.mat.adj.1600) <- list(sp1600, sp1600) #assign names to the rows and coloumns on new matrix
  for(m in 1:length(sp1600)){
    for(n in 1:length(sp1600))
    {
      obs.mat.adj.1600[m,n] <- length(which(obs_mat.1600[,paste(sp1600[m])] == 1 & obs_mat.1600[,paste(sp1600[n])] == 1))
    }
  }
  
  
  
  # g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
  # plot(g800, edge.width=E(g800)$weight)
  
  
  sp.rich.1600 <- length(sp1600)
  sp.rich.1600
  
  
  diag(obs.mat.adj.1600) <- 0
  obs.net.igraph.1600 <- graph.adjacency(obs.mat.adj.1600, mode = "undirected", weighted = TRUE)
  
  obs.density.1600 <- round(edge_density(obs.net.igraph.1600), 2)
  obs.density.1600
  
  # species properties: weighted degree 
  obs.degree.1600 <- degree(obs.net.igraph.1600)
  sort(obs.degree.1600, decreasing = T)
  hist(obs.degree.1600, 10)


############ 2000m ############# 
  data2000m <- splited_fc[["2000"]]  
  
  #removing flocks with just one species. since they are not mixed!
  Splevel_data.2000 <- data2000m %>%
    group_by(flock_id) %>%
    mutate(n_species = length(unique(species))) %>%
    ungroup() %>%
    filter(n_species > 1)
  
  
  
  # 2000m
  sp2000 <-  sort(unique(Splevel_data.2000$species))
  fl2000 <- sort(unique(Splevel_data.2000$flock_id))
  
  
  
  # flock-by-species matrix 2000m
  obs_mat.2000 <- matrix(0, nrow = length(fl2000), ncol = length(sp2000))
  dimnames(obs_mat.2000) <- list(fl2000, sp2000)
  for(i in 1:length(fl2000)){
    temp <- subset(fc, flock_id == fl2000[i])
    obs_mat.2000[i, which(colnames(obs_mat.2000) %in% temp$species)] <- 1
  }
  
  
  # species-by-species matrix
  obs.mat.adj.2000 <- matrix(0, length(sp2000), length(sp2000)); #created a empty matrix
  dimnames(obs.mat.adj.2000) <- list(sp2000, sp2000) #assign names to the rows and coloumns on new matrix
  for(m in 1:length(sp2000)){
    for(n in 1:length(sp2000))
    {
      obs.mat.adj.2000[m,n] <- length(which(obs_mat.2000[,paste(sp2000[m])] == 1 & obs_mat.2000[,paste(sp2000[n])] == 1))
    }
  }
  
  # g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
  # plot(g800, edge.width=E(g800)$weight)
  
  
  sp.rich.2000 <- length(sp2000)
  sp.rich.2000
  
  
  diag(obs.mat.adj.2000) <- 0
  obs.net.igraph.2000 <- graph.adjacency(obs.mat.adj.2000, mode = "undirected", weighted = TRUE)
  
  obs.density.2000 <- round(edge_density(obs.net.igraph.2000), 2)
  obs.density.2000
  
  # species properties: weighted degree 
  obs.degree.2000 <- degree(obs.net.igraph.2000)
  sort(obs.degree.2000, decreasing = T)
  hist(obs.degree.2000, 10)
  

  
  
############ 2400m #############   
  data2400m <- splited_fc[["2400"]]  
  
  #removing flocks with just one species. since they are not mixed!
  Splevel_data.2400 <- data2400m %>%
    group_by(flock_id) %>%
    mutate(n_species = length(unique(species))) %>%
    ungroup() %>%
    filter(n_species > 1)
  
  
  
  # 2400m
  sp2400 <-  sort(unique(Splevel_data.2400$species))
  fl2400 <- sort(unique(Splevel_data.2400$flock_id))
  
  
  
  # flock-by-species matrix 2400m
  obs_mat.2400 <- matrix(0, nrow = length(fl2400), ncol = length(sp2400))
  dimnames(obs_mat.2400) <- list(fl2400, sp2400)
  for(i in 1:length(fl2400)){
    temp <- subset(fc, flock_id == fl2400[i])
    obs_mat.2400[i, which(colnames(obs_mat.2400) %in% temp$species)] <- 1
  }
  
  
  # species-by-species matrix
  obs.mat.adj.2400 <- matrix(0, length(sp2400), length(sp2400)); #created a empty matrix
  dimnames(obs.mat.adj.2400) <- list(sp2400, sp2400) #assign names to the rows and coloumns on new matrix
  for(m in 1:length(sp2400)){
    for(n in 1:length(sp2400))
    {
      obs.mat.adj.2400[m,n] <- length(which(obs_mat.2400[,paste(sp2400[m])] == 1 & obs_mat.2400[,paste(sp2400[n])] == 1))
    }
  }
  
  
  
  # g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
  # plot(g800, edge.width=E(g800)$weight)
  
  
  sp.rich.2400 <- length(sp2400)
  sp.rich.2400
  
  
  diag(obs.mat.adj.2400) <- 0
  obs.net.igraph.2400 <- graph.adjacency(obs.mat.adj.2400, mode = "undirected", weighted = TRUE)
  
  obs.density.2400 <- round(edge_density(obs.net.igraph.2400), 2)
  obs.density.2400
  

  # species properties: weighted degree 
  obs.degree.2400 <- degree(obs.net.igraph.2400)
  sort(obs.degree.2400, decreasing = T)
  hist(obs.degree.2400, 10)
  


############ 2800m ############# 
  data2800m <- splited_fc[["2800"]]  
  
  #removing flocks with just one species. since they are not mixed!
  Splevel_data.2800 <- data2800m %>%
    group_by(flock_id) %>%
    mutate(n_species = length(unique(species))) %>%
    ungroup() %>%
    filter(n_species > 1)
  
  
  
  # 2800m
  sp2800 <-  sort(unique(Splevel_data.2800$species))
  fl2800 <- sort(unique(Splevel_data.2800$flock_id))
  
  
  
  # flock-by-species matrix 2800m
  obs_mat.2800 <- matrix(0, nrow = length(fl2800), ncol = length(sp2800))
  dimnames(obs_mat.2800) <- list(fl2800, sp2800)
  for(i in 1:length(fl2800)){
    temp <- subset(fc, flock_id == fl2800[i])
    obs_mat.2800[i, which(colnames(obs_mat.2800) %in% temp$species)] <- 1
  }
  
  
  # species-by-species matrix
  obs.mat.adj.2800 <- matrix(0, length(sp2800), length(sp2800)); #created a empty matrix
  dimnames(obs.mat.adj.2800) <- list(sp2800, sp2800) #assign names to the rows and coloumns on new matrix
  for(m in 1:length(sp2800)){
    for(n in 1:length(sp2800))
    {
      obs.mat.adj.2800[m,n] <- length(which(obs_mat.2800[,paste(sp2800[m])] == 1 & obs_mat.2800[,paste(sp2800[n])] == 1))
    }
  }
  
  
  
  # g800<- graph_from_adjacency_matrix(obs.mat.adj, mode="undirected", weighted = TRUE, diag = FALSE )
  # plot(g800, edge.width=E(g800)$weight)
  
  
  sp.rich.2800 <- length(sp2800)
  sp.rich.2800
  
  
  diag(obs.mat.adj.2800) <- 0
  obs.net.igraph.2800 <- graph.adjacency(obs.mat.adj.2800, mode = "undirected", weighted = TRUE)
  
  obs.density.2800 <- round(edge_density(obs.net.igraph.2800), 2)
  obs.density.2800
  
  # species properties: weighted degree 
  obs.degree.2800 <- degree(obs.net.igraph.2800)
  sort(obs.degree.2800, decreasing = T)
  hist(obs.degree.2800, 10)
  

############### saving matrix as csv and i graph object as GraphML file #################
 
write.csv(obs.mat.adj.2800,here::here("..", "..", "data_files","Species co_occurence matrix", "species_species_matrix_2800m.csv"), row.names = TRUE)

write_graph(obs.net.igraph.2800,here::here("..", "..", "data_files", "igraph_object","obs.net.igraph.2800.graphml"), format = "graphml")



###################### converting the matrix into coloum data ################
  m800<-read.csv(here("..", "..", "data_files","Species co_occurence matrix", "species_species_matrix_800m.csv"),row.names = 1)
  
  # Assuming m800 is your original matrix
  df_long <- m800 %>%
    as.data.frame(check.names = FALSE) %>%
    # Explicitly convert all column names to character strings
    setNames(as.character(names(.))) %>%
    tibble::rownames_to_column(var = "species1") %>%
    pivot_longer(
      cols = -species1,
      names_to = "species2",
      values_to = "weight"
    )
  
  # Now, filter out self-interactions and zero weights
  # (This is a good practice for preparing the interaction data frame)
  interaction_df <- df_long %>%
    filter(species1 != species2, weight != 0)
  
  # Print the final, cleaned data frame
  print(interaction_df)
  
  
############# Interactive network ######################
  # Compute the most frequent canopy_level for each species in data2800m
  species_canopy <- data1600m %>%
    group_by(species) %>%
    summarise(strata = names(which.max(table(strata)))) %>%
    ungroup()
  
  
  species_canopy <- data1600m %>%
    group_by(species) %>%
    summarise(strata = paste(unique(strata), collapse = ", ")) %>%
    ungroup()
  
  colnames(species_canopy)[colnames(species_canopy) == "strata"] <- "canopy_level"
  
  species_canopy<- species_canopy %>%
    mutate(
      canopy_level = case_when(
        # Check for 'C, MU' or 'MU, C' (with or without spaces) and standardize
        canopy_level == "MU, C" ~ "C, MU",
        canopy_level == "MU,C"  ~ "C, MU", # Catch cases without a space
        TRUE ~ canopy_level # Keep all other values as they are
      )
    )
  
  g1600<-obs.net.igraph.1600
  
  # Ensure the species order matches the graph's vertex names (which are sorted as in sp1600)
  species_canopy <- species_canopy[match(V(g1600)$name, species_canopy$species), ]
  
  # Add as a vertex attribute to the graph
  V(g1600)$canopy_level <- species_canopy$canopy_level
  
  
  ###plot
  # Convert igraph to visNetwork format
  vis_data <- toVisNetworkData(g1600)
  
  # Customize node colors based on canopy_level (as before)
  canopy_levels <- unique(V(g1600)$canopy_level)
  num_levels <- length(canopy_levels)
  library(RColorBrewer)
  if (num_levels <= 9) {
    canopy_colors <- brewer.pal(num_levels, "Set1")
  } else {
    canopy_colors <- rainbow(num_levels)
  }
  color_map <- setNames(canopy_colors, canopy_levels)
  
  # Add color to nodes
  vis_data$nodes$color <- color_map[vis_data$nodes$canopy_level]
  
  # Set a fixed node size (no weighting by degree) - adjust the number as needed for visibility
  vis_data$nodes$size <- 10  # Fixed size; increase/decrease as preferred (default visNetwork size is 25)
  
  # Optional: Add edge width based on weight
  vis_data$edges$width <- sqrt(vis_data$edges$weight) * 2  # Scale for visibility
  
  # Create the interactive plot with low movement (stable physics)
  visNetwork(nodes = vis_data$nodes, edges = vis_data$edges) %>%
    visPhysics(stabilization = TRUE,  # Enable stabilization to reduce movement
               enabled = TRUE,        # Keep physics on for some interactivity
               solver = "forceAtlas2Based",  # A stable solver
               forceAtlas2Based = list(gravitationalConstant = -50,  # Low gravity for less pull
                                       centralGravity = 0.01,       # Low central force
                                       springLength = 100,          # Longer springs for stability
                                       springConstant = 0.08,       # Low spring force
                                       damping = 0.4,               # High damping to slow movement
                                       avoidOverlap = 0)) %>%       # Prevent overlap but keep stable
    visEdges(color = list(color = "rgba(0,0,0,0.2)", highlight = "yellow"),  # Semi-transparent edges
             smooth = FALSE) %>%  # Straight edges for simplicity
    visNodes(shadow = TRUE, borderWidth = 1) %>%  # Add shadows and borders for better look
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%  # Enable node selection and highlighting
    visLegend(addNodes = data.frame(label = canopy_levels, color = canopy_colors), 
              useGroups = FALSE, position = "right")  # Add legend for canopy levels

  

  

  
  
  