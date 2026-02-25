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


#########adding abundance of species ###########
fc <- fc %>%
  mutate(abundance = ifelse(!is.na(min_ind), min_ind, max_ind))

##############subset of fc data############
fcibj<- fc %>%
   select(flock_id,elevation,species,canopy_level,abundance,strata)

######### Mean flock size and number of flocks sampled per elevation ##################

mean_flock_size_per_elevation <- fcibj %>%
  group_by(elevation, flock_id) %>%
  summarise(flock_size = n(), .groups = "drop") %>%
  group_by(elevation) %>%
  summarise(
    mean_flock_size = mean(flock_size),
    n_flocks = n()
  )

mean_flock_size_per_elevation



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
 
   # Assuming 'species_matrix' is your data frame or matrix object
  
write.csv(obs.mat.adj.2800,here::here("..", "..", "data_files","Species co_occurence matrix", "species_species_matrix_2800m.csv"), row.names = TRUE)

write_graph(obs.net.igraph.2800,here::here("..", "..", "data_files", "igraph_object","obs.net.igraph.2800.graphml"), format = "graphml")



###################### converting the matrix into coloum data ################
  m800<-read.csv(here("..", "..", "data_files","Edge_list_Adjacency_list", "species_species_matrix_800m.csv"),row.names = 1)
  
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

  
############## 

{
#making a dataframe and storing at overall per eleavation
richness<- c(sp.rich.800,sp.rich.1200,sp.rich.1600,sp.rich.2000,sp.rich.2400,sp.rich.2800)
density.elevation<-c(obs.density.800,obs.density.1200,obs.density.1600,obs.density.2000,obs.density.2400,obs.density.2800)# this denotes the density of a network, which is a measure of how close it is to being a complete graph is
elevation<-sort(unique(fc$elevation))
concetdness.df<-data.frame(density.elevation,elevation,richness)# at elevation level combined




#making a dataframe with specific specific across elevation and their individual interaction
deg.2800<- degree(obs.net.igraph.2800)
deg.df.2800 <- data.frame(species = names(deg.2800),degree  = as.numeric(deg.2800),elevation = 2800)
deg.df.2800 <- deg.df.2800 %>%
  mutate(condensed = degree /length(deg.df.2800$species))

deg.2400<- degree(obs.net.igraph.2400)
deg.df.2400 <- data.frame(species = names(deg.2400),degree  = as.numeric(deg.2400),elevation = 2400)
deg.df.2400 <- deg.df.2400 %>%
  mutate(condensed = degree /length(deg.df.2400$species))

deg.2000<- degree(obs.net.igraph.2000)
deg.df.2000 <- data.frame(species = names(deg.2000),degree  = as.numeric(deg.2000),elevation = 2000)
deg.df.2000 <- deg.df.2000 %>%
  mutate(condensed = degree /length(deg.df.2000$species))

deg.1600<- degree(obs.net.igraph.1600)
deg.df.1600 <- data.frame(species = names(deg.1600),degree  = as.numeric(deg.1600),elevation = 1600)
deg.df.1600 <- deg.df.1600 %>%
  mutate(condensed = degree /length(deg.df.1600$species))

deg.1200<- degree(obs.net.igraph.1200)
deg.df.1200 <- data.frame(species = names(deg.1200),degree  = as.numeric(deg.1200),elevation = 1200)
deg.df.1200 <- deg.df.1200 %>%
  mutate(condensed = degree /length(deg.df.1200$species))

deg.800<- degree(obs.net.igraph.800)
deg.df.800 <- data.frame(species = names(deg.800),degree  = as.numeric(deg.800),elevation = 800)
deg.df.800 <- deg.df.800 %>%
  mutate(condensed = degree /length(deg.df.800$species))


deg.all.sp<-rbind(deg.df.800,deg.df.1200,deg.df.1600,deg.df.2000,deg.df.2400,deg.df.2800)# lengths of richness and degree was not matched, it was not working, check again
deg.all.sp<- left_join(deg.all.sp, concetdness.df, by = "elevation")





#Ploting histograms for distrubition across elevation
ggplot(deg.all.sp, aes(x = degree)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~elevation) +               # separate panel per elevation
#  xlim(0.3, 0.7) +                          # same x-axis across all panels
  theme_minimal() +
  labs(title = "Species-Level Connectivity in Elevational Networks", #Degree distribution across elevation 
       x = "Connections(Degree)",
       y = "Number of Species")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # centered & styled
  )


# code is showing Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
ggplot(deg.all.sp, aes(x = degree)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~elevation) +               # separate panel per elevation
  #  xlim(0.3, 0.7) +                          # same x-axis across all panels
  theme_minimal() +
  labs(title = "Species-Level Connectivity in Elevational Networks", #Degree distribution across elevation 
       x = "Connections(Degree)",
       y = "Number of Species")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # centered & styled
  )





# code worked and ploted 
ggplot(concetdness.df, aes(x = factor(elevation), y = density.elevation, fill = factor(elevation))) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  theme_minimal() +
  labs(x = "Elevation", y = "Density") +
  guides(fill = "none")


}# friday meeting

{
 
deg.2800 <- degree(obs.net.igraph.2800)
deg.df.2800 <- data.frame(
    species = names(deg.2800),
    degree  = as.numeric(deg.2800),
    elevation = 2800
  )

deg.df.all<-rbind(deg.df.800,deg.df.1200,deg.df.1600,deg.df.2000,deg.df.2000,deg.df.2800) 
  
}# it has all the species and its interaction with the degree(interaction)

{
cl.800<- cluster_louvain(obs.net.igraph.800)
 plot(obs.net.igraph.800,
           layout = layout_with_fr(obs.net.igraph.800),
             vertex.color = membership(cl.800),
               vertex.size = 8,
             vertex.label.cex = 0.6,
            edge.color = "gray80")
}#cluster_louvain

{
plot(obs.net.igraph.800, 
     layout = layout_with_fr(obs.net.igraph.800), # you can play with this parameter to make it look different.
     # vertex.label = NA,
     vertex.color = rgb(0,0,1, 0.2),
     vertex.size = obs.degree/10,
     vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0, edge.color = rgb(0,0,0, alpha = 0.2),
     edge.width = sqrt(E(obs.net.igraph.800)$weight) # made for better visualisation. You can use any scaling you'd prefer
)
}# network plot from Akshay bahiya code

{#Simple basic use
ln<-layout_nicely(obs.net.igraph.800)
plot(obs.net.igraph.800, layout= ln)
} # name it 

{plot(obs.net.igraph.2800, 
     layout = layout_nicely(obs.net.igraph.2800), # you can play with this parameter to make it look different.
     # vertex.label = NA,
     vertex.color = rgb(0,0,1, 0.2),
     vertex.size = obs.degree/10,vertex.label = NA,
     # vertex.label.color="black", 
     # vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0, edge.color = rgb(0,0,0, alpha = 0.2),
     edge.width = sqrt(E(obs.net.igraph.2800)$weight) # made for better visualisation. You can use any scaling you'd prefer
,main = "Network Plot 2800m")
}#without any labels

{
  # #density by degree grouped by elevation
  # ggplot(deg.all.sp, aes(x = condensed, color = as.factor(elevation))) +
  #        geom_density() +
  #        labs(
  #            title = "Degree Distribution by Elevation",
  #              x = "Degree",
  #              y = "Density",
  #              color = "Elevation"
  #         ) + theme_minimal()

# Plot done after the update from 22nd meetings
ggplot(deg.all.sp, aes(x = condensed, color = as.factor(elevation)))+
    geom_density(size = 1) +
    scale_color_manual(
      values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ffff33", "#ff7f00")
    ) +
    labs(
      title = "Degree Distribution Across Elevations",
      x = "Range",
      y = "Density",
      color = NULL
    ) +
    theme_minimal()

}#additional plots after #22-09-25 meeting

{
effort <- fc %>%
  group_by(elevation, date) %>%          # group by elevation and actual date
  summarise(
    total_flocks = n_distinct(flock_id)  # count of unique flocks observed that day
  ) %>%
  ungroup()
 
  
   
  ggplot(effoor, aes(x = factor(elevation), y = total_flocks)) +
    geom_boxplot(fill = "skyblue") +
    labs(x = "Elevation", y = "Total Flocks per Day") +
    theme_minimal()
  
  
}# effort at each elavtion

{  
  #ploting normalised degree of species for elevation comined tosee any patterns across elavtion or in elavation
  ggplot(deg.all.sp, aes(x = condensed, color = as.factor(elevation)))+
    geom_density(linewidth = 1) +
    scale_color_manual(
      # values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
      values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00", "#4d4d4d")
    ) +
    labs(
      title = "Species-Level Connectivity in Elevational Networks",
      x = "Normalised Degree",
      y = "Density",
      color = NULL
    ) +
    theme_minimal()
  
  
  
  
#ploting interaction(degree) of species for elevation combined to see any patterns across elavtion or in elavation without normalised
  ggplot(deg.all.sp, aes(x = degree, color = as.factor(elevation)))+
    geom_density(size = 1) +
    scale_color_manual(
      # values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
      values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00", "#4d4d4d")
    ) +
    labs(
      title = "Degree Distribution Across Elevations",
      x = "Degree",
      y = "Density",
      color = NULL
    ) +
    theme_minimal()
  
  
#shaded by group, it worked but was not visually apealling and informative
  
  # ggplot(deg.all.sp, aes(x = condensed, fill = as.factor(elevation))) +
  #   geom_density(alpha = 0.3) + # Add shading with alpha for transparency
  #   scale_fill_manual(
  #     values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#4d4d4d")
  #   ) +
  #   labs(
  #     title = "Degree Distribution Across Elevations",
  #     x = "Normalised degree",
  #     y = "Density",
  #     fill = "Elevation" # Change 'color' to 'fill' for the legend title
  #   ) +
  #   theme_minimal() +
  #   guides(
  #     fill = guide_legend(override.aes = list(shape = 21, size = 5, color = "black"))
  #   ) # Override the legend key to show circles 
  # 
  
  
  
  
# geom point plot for Conectdness by Elevation
  ggplot(concetdness.df, aes(x = elevation, y = density.elevation, size = richness, color = as.factor(elevation))) +
    geom_point() +
    scale_color_manual(
      values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#252525")
    ) +
    labs(
      title = "Network Connectdness by Elevation and Richness",
      x = "Elevation",
      y = "Network Connectdness",
      color = "Elevation", # Sets the legend title for color
      size = "Richness"   # Sets the legend title for size
    ) +
    theme_minimal()
  
#OR
 
  ggplot(concetdness.df, aes(x = elevation, y = density.elevation, 
                             size = richness, color = as.factor(elevation))) +
    geom_point() +
    scale_color_manual(
      values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#252525")
    ) +
     scale_size_continuous(name = "Richness", range = c(5, 10), guide = "legend")  +  # 👈 control min/max point size here
    labs(
      title = "Network Connectedness by Elevation and Richness",
      x = "Elevation",
      y = "Network Connectedness",
      color = "Elevation",
      size = "Richness"
    ) +
    theme_minimal()
  
}# plot scripts after 22ndSep meeting code

  
#### jdjeeofjwoijoi ####
  #ffihufhuwihreh
  #EJKBEW
  
  

  
  
  