#SPECIES–ELEVATION NETWORK

library(dplyr)
library(visNetwork)

# -----------------------------
# 1. Prepare data
# -----------------------------

df_net <- msf %>%
  select(species, elevation) %>%
  distinct()

# -----------------------------
# 2. Create nodes
# -----------------------------

species_nodes <- data.frame(
  id = unique(df_net$species),
  label = unique(df_net$species),
  group = "Species"
)

elevation_nodes <- data.frame(
  id = as.character(unique(df_net$elevation)),
  label = as.character(unique(df_net$elevation)),
  group = "Elevation"
)

nodes <- bind_rows(species_nodes, elevation_nodes)

# -----------------------------
# 3. Create edges
# -----------------------------

edges <- df_net %>%
  mutate(from = species,
         to   = as.character(elevation)) %>%
  select(from, to)

# -----------------------------
# 4. Node styling
# -----------------------------

nodes$color <- ifelse(nodes$group == "Species",
                      "#28B463",
                      "#ffff99")

# Increase node size
nodes$size <- ifelse(nodes$group == "Species", 25, 35)

# -----------------------------
# 5. Network
# -----------------------------

visNetwork(nodes, edges, height = "800px", width = "100%") %>%
  
  visNodes(
    font = list(size = 22),
    borderWidth = 2
  ) %>%
  
  visEdges(
    smooth = list(enabled = TRUE, type = "dynamic"),
    color = list(color = "#848484", highlight = "red")
  ) %>%
  
  visOptions(
    highlightNearest = list(enabled = TRUE, hover = TRUE),  # 🔥 highlight on hover
    nodesIdSelection = TRUE
  ) %>%
  
  visInteraction(
    hover = TRUE
  ) %>%
  
  visPhysics(
    solver = "forceAtlas2Based",
    stabilization = list(enabled = TRUE, iterations = 1500),
    forceAtlas2Based = list(
      gravitationalConstant = -200,
      centralGravity = 0.01,
      springLength = 350,
      springConstant = 0.03
    )
  )