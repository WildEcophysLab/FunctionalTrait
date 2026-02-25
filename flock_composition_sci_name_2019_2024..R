# final one 
library(dplyr)
library(readr)
library(stringr)
library(here)
library(ggplot2)
library(tidyr)
######### reading files ###############
# Field datafiles
smf<-read.csv(here("..","..","data_files", "sarthak_2019_20_mf_data_updated.csv"))
fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
# Name fixes
fc <- fc%>%
  mutate(species = str_replace_all(species, "[']", ""))  # remove all ' 

fc$species <- sub("YellowBelliedFantail", "YellowBelliedFairyFantail", fc$species) # Usage: fc$column <- sub("old_name", "new_name", fc$column)

#changing gray to grey 
ebird_taxa$PRIMARY_COM_NAME <- gsub("Gray", "Grey", ebird_taxa$PRIMARY_COM_NAME)

#e-bird Taxonomy 2024 list
ebird_taxa<-read.csv(here("..","..","field_raw_datasheets", "eBird_taxonomy_v2024.csv"))

# AVO net trait data and removing unessary coloums 
avo<-read.csv(here("..","..","field_raw_datasheets", "AVONET_Raw_Data.csv"))
# filtering
columns_to_keep <- c("common_name", "Avibase.ID", "Species1_BirdLife", "Species2_eBird", "eBird.species.group", "Species3_BirdTree", "Data.type", "Source",
                     "Specimen.number", "Sex", "Age", "Locality", "Country_WRI", "Country", "Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", "Beak.Depth", 
                     "Tarsus.Length", "Wing.Length", "Kipps.Distance", "Secondary1", "Hand.wing.Index", "Tail.Length", "Measurer", "Protocol", "Publication")
avo<- avo %>%
  select(any_of(columns_to_keep))
# this is supplementary from AVOnet using for species which has problems in the main trait files
avo_supply<-read.csv(here("..","..","field_raw_datasheets", "Supplementary_dataset.Trait_Data.csv"))


#e-bird file modification 
# removing Unnecessary coloumn
columns_to_keep <- c("PRIMARY_COM_NAME","SCI_NAME","ORDER","FAMILY","SPECIES_GROUP")
ebird_taxa<- ebird_taxa %>%
  select(any_of(columns_to_keep))
# fixing the names
ebird_taxa <- ebird_taxa %>%
  mutate(PRIMARY_COM_NAME = str_replace_all(PRIMARY_COM_NAME, "[- ]", " "),  # replace - and space with space
         PRIMARY_COM_NAME = str_to_title(PRIMARY_COM_NAME),                 # title case
         PRIMARY_COM_NAME = str_replace_all(PRIMARY_COM_NAME, " ", ""))  # remove all spaces (PascalCase)
#changing gray to grey 
ebird_taxa$PRIMARY_COM_NAME <- gsub("Gray", "Grey", ebird_taxa$PRIMARY_COM_NAME)



##################  filtering ebird as per flock composition 
# step 1.1 filtering ebird as per smf 
ebird_smf <- ebird_taxa %>%
  filter(PRIMARY_COM_NAME %in% smf$species)
#OR
ebird_smf <- ebird_taxa[ebird_taxa$PRIMARY_COM_NAME %in% smf$species,]

# step 1.2 filtering ebird as per fc
ebird_fc <- ebird_taxa %>%
  filter(PRIMARY_COM_NAME %in% fc$species)
#OR
ebird_fc <- ebird_taxa[ebird_taxa$PRIMARY_COM_NAME %in% fc$species,]



######### adding scintific names to smf ##########

# Create clean lookup: common name → scientific name
sci_lookup <- ebird_smf %>%
  distinct(PRIMARY_COM_NAME, SCI_NAME) %>%
  filter(!is.na(PRIMARY_COM_NAME))

# Join scientific names to smf
smf_with_sci <- smf %>%
  left_join(
    sci_lookup,
    by = c("species" = "PRIMARY_COM_NAME")
  )

# Check how many matched
cat("Total rows in smf:", nrow(smf), "\n")
cat("Rows with sci_name:", sum(!is.na(smf_with_sci$SCI_NAME)), "\n")
cat("Unmatched species:", sum(is.na(smf_with_sci$SCI_NAME)), "\n")

# See which species didn't match (if any)
unmatched <- smf_with_sci %>%
  filter(is.na(SCI_NAME)) %>%
  distinct(species)

print(unmatched)


#manually fixing the scientific names which didnot matches to avonet

# Your manual corrections
manual_corrections <- tibble::tribble(
  ~species,                    ~SCI_NAME_corrected,
  "RufousHeadedParrotbill",    "Psittiparus bakeri",
  "PaleBilledParrotbill",      "Paradoxornis atrosuperciliaris",
  "GreyHeadedWarbler",         "Basileuterus griseiceps",
  "WhiteBrowedShrikeBabbler",  "Pteruthius aeralatus",
  "StriatedBulbul",            "Alcurus striatus",
  "GreyHeadedParrotbill",      "Paradoxornis gularis"
)

# Apply corrections to smf
smf_final <- smf_with_sci %>%
  left_join(manual_corrections, by = "species") %>%
  mutate(
    SCI_NAME = coalesce(SCI_NAME_corrected, SCI_NAME)
  ) %>%
  select(-SCI_NAME_corrected)  # remove temp column

# Verify the changes
smf_final %>%
  filter(species %in% manual_corrections$species) %>%
  distinct(species, SCI_NAME)



############## fc [2024-2025] #####################

#for fc

#tidyverse
### simple code for diltering avonet and adding common name in there #
# Step 1: Create lookup_fc table from eBird
lookup_fc <- ebird_fc %>% 
  distinct(SCI_NAME, PRIMARY_COM_NAME)

# Step 2: Priority-based matching (Species2_eBird first, then eBird.species.group)
avo_fc <- avo %>%
  # First priority: match Species2_eBird
  left_join(lookup_fc, by = c("Species2_eBird" = "SCI_NAME")) %>%
  rename(common_p1 = PRIMARY_COM_NAME) %>%
  
  # Second priority: match eBird.species.group (fallback)
  left_join(lookup_fc, by = c("eBird.species.group" = "SCI_NAME")) %>%
  rename(common_p2 = PRIMARY_COM_NAME) %>%
  
  # Select priority: use p1 if available, else p2
  mutate(
    common_name = coalesce(common_p1, common_p2)
    # Alternative base R: ifelse(!is.na(common_p1), common_p1, common_p2)
  ) %>%
  
  # Clean up helper columns
  select(-common_p1, -common_p2) %>%
  
  # Optional: keep only successfully matched rows
  filter(!is.na(common_name))

#### only selcting species which is present in fc
avonet_fc <- avo_fc[avo_fc$common_name %in% fc$species,]

# Giving common names for species which sci-names were not matched
setdiff(unique(fc$species),unique(avonet_fc$common_name))

#### only selcting species which is present in fc 
#avonet_fc <- avo_with_species_fc[avo_with_species_fc$species %in% fc$species,]

##### checking which is not in the avo net yet 
#sp_avo_fc <- as.character(avo_with_species_fc$Species2_eBird)
#sp_ebird_fc <- as.character(ebird_fc$SCI_NAME)
#diff_avo_fc<-setdiff(unique(sp_ebird_fc), unique(sp_avo_fc))

#common name
com_avo_fc <- as.character(avonet_fc$common_name)
com_ebird_fc <- as.character(ebird_fc$PRIMARY_COM_NAME)

diff_avo_fc<-setdiff(unique(com_ebird_fc), unique(com_avo_fc))

diff_avo_fc<-setdiff(unique(com_ebird_fc), unique((avonet_fc$common_name)))

# Extracting the missing scientific names
# Get scientific names for the 6 missing common names
missing_sci <- ebird_fc %>%
  filter(PRIMARY_COM_NAME %in% diff_avo_fc) %>%
  distinct(PRIMARY_COM_NAME, SCI_NAME, FAMILY)

print(missing_sci)



############### briged and added the scientificnames
# Taxonomic Bridge Table for non-matchables
taxonomic_bridge_fc <- data.frame(
  manual_scientific = c("Pteruthius aeralatus","Paradoxornis unicolor","Paradoxornis gularis","Paradoxornis ruficeps","Dicaeum melanozanthum"),# what it's called in avo got the names by manually looking into it
  ebird_scientific = c("Pteruthius aeralatus","Paradoxornis unicolor","Paradoxornis gularis","Paradoxornis ruficeps","Pachyglossa melanozantha"),   # what it's called in ebird
  common_name = c("WhiteBrowedShrikeBabbler","BrownParrotbill","GreyHeadedParrotbill","WhiteBreastedParrotbill","YellowBelliedFlowerpecker")         # target common name
)

# 1. Prepare lookup_fcs
ebird_lookup_fc <- ebird_fc %>%
  distinct(SCI_NAME, PRIMARY_COM_NAME) %>%
  filter(!is.na(SCI_NAME))

bridge_lookup_fc <- taxonomic_bridge_fc %>%
  distinct(manual_scientific, ebird_scientific) %>%
  filter(!is.na(manual_scientific))

# 2. Enrich with priority matching + bridge fallback
avo_fc <- avo %>%
  # Priority 1: Species2_eBird (direct)
  left_join(
    ebird_lookup_fc %>% rename(common_p1 = PRIMARY_COM_NAME),
    by = c("Species2_eBird" = "SCI_NAME")
  ) %>%
  
  # Priority 2: eBird.species.group (direct)
  left_join(
    ebird_lookup_fc %>% rename(common_p2 = PRIMARY_COM_NAME),
    by = c("eBird.species.group" = "SCI_NAME")
  ) %>%
  
  # Priority 3: Species1_BirdLife (direct first, then bridge)
  left_join(
    ebird_lookup_fc %>% rename(common_p3_direct = PRIMARY_COM_NAME),
    by = c("Species1_BirdLife" = "SCI_NAME")
  ) %>%
  left_join(
    bridge_lookup_fc %>% rename(ebird_p3 = ebird_scientific),
    by = c("Species1_BirdLife" = "manual_scientific")
  ) %>%
  left_join(
    ebird_lookup_fc %>% rename(common_p3_bridge = PRIMARY_COM_NAME),
    by = c("ebird_p3" = "SCI_NAME")
  ) %>%
  
  # Priority 4: Species3_BirdTree (direct first, then bridge)
  left_join(
    ebird_lookup_fc %>% rename(common_p4_direct = PRIMARY_COM_NAME),
    by = c("Species3_BirdTree" = "SCI_NAME")
  ) %>%
  left_join(
    bridge_lookup_fc %>% rename(ebird_p4 = ebird_scientific),
    by = c("Species3_BirdTree" = "manual_scientific")
  ) %>%
  left_join(
    ebird_lookup_fc %>% rename(common_p4_bridge = PRIMARY_COM_NAME),
    by = c("ebird_p4" = "SCI_NAME")
  ) %>%
  
  # 3. Resolve final names with full traceability
  mutate(
    common_name = coalesce(
      common_p1,           # Species2_eBird
      common_p2,           # eBird.species.group
      common_p3_direct,    # BirdLife direct
      common_p3_bridge,    # BirdLife via bridge
      common_p4_direct,    # BirdTree direct  
      common_p4_bridge     # BirdTree via bridge
    ),
    
    match_source = case_when(
      !is.na(common_p1) ~ "Species2_eBird (direct)",
      !is.na(common_p2) ~ "eBird.species.group (direct)",
      !is.na(common_p3_direct) ~ "Species1_BirdLife (direct)",
      !is.na(common_p3_bridge) ~ "Species1_BirdLife (bridged)",
      !is.na(common_p4_direct) ~ "Species3_BirdTree (direct)",
      !is.na(common_p4_bridge) ~ "Species3_BirdTree (bridged)",
      TRUE ~ "Unmatched"
    ),
    
    # Track which eBird scientific name was ultimately used
    ebird_sci_resolved = case_when(
      !is.na(common_p1) ~ Species2_eBird,
      !is.na(common_p2) ~ eBird.species.group,
      !is.na(common_p3_direct) ~ Species1_BirdLife,
      !is.na(common_p3_bridge) ~ ebird_p3,
      !is.na(common_p4_direct) ~ Species3_BirdTree,
      !is.na(common_p4_bridge) ~ ebird_p4,
      TRUE ~ NA_character_
    )
  ) %>%
  
  # 4. Clean up
  select(-matches("^common_p[0-9]"), -matches("^ebird_p[0-9]"))

# 5. Check results
table(avo_fc$match_source, useNA = "ifany")


# filtering only species which is in fc

avo_fc <- avo_fc[avo_fc$common_name %in% fc$species,]

############ This species has Nas in trait 
# Drop rows with any NA
avo_clean_fc <- avo_fc %>%
  drop_na(Beak.Length_Culmen:Tail.Length)



############### smf[2019 snd 2020]  ##############
#tidyverse
### simple code for diltering avonet and adding common name in there #
# Step 1: Create lookup table from eBird
lookup <- ebird_smf %>% 
  distinct(SCI_NAME, PRIMARY_COM_NAME)

# Step 2: Priority-based matching (Species2_eBird first, then eBird.species.group)
avo_final <- avo %>%
  # First priority: match Species2_eBird
  left_join(lookup, by = c("Species2_eBird" = "SCI_NAME")) %>%
  rename(common_p1 = PRIMARY_COM_NAME) %>%
  
  # Second priority: match eBird.species.group (fallback)
  left_join(lookup, by = c("eBird.species.group" = "SCI_NAME")) %>%
  rename(common_p2 = PRIMARY_COM_NAME) %>%
  
  # Select priority: use p1 if available, else p2
  mutate(
    common_name = coalesce(common_p1, common_p2)
    # Alternative base R: ifelse(!is.na(common_p1), common_p1, common_p2)
  ) %>%
  
  # Clean up helper columns
  select(-common_p1, -common_p2) %>%
  
  # Optional: keep only successfully matched rows
  filter(!is.na(common_name))

#### only selcting species which is present in smf
avonet_smf.1 <- avo_final[avo_final$common_name %in% smf$species,]

# Giving common names for species which sci-names were not matched
setdiff(unique(smf$species),unique(avonet_smf.1$common_name))

#### only selcting species which is present in smf 
#avonet_smf <- avo_with_species_smf[avo_with_species_smf$species %in% smf$species,]

##### checking which is not in the avo net yet 
#sp_avo_smf <- as.character(avo_with_species_smf$Species2_eBird)
#sp_ebird_smf <- as.character(ebird_smf$SCI_NAME)
#diff_avo_smf<-setdiff(unique(sp_ebird_smf), unique(sp_avo_smf))

#common name
com_avo_smf <- as.character(avonet_smf.1$common_name)
com_ebird_smf <- as.character(ebird_smf$PRIMARY_COM_NAME)

diff_avo_smf<-setdiff(unique(com_ebird_smf), unique(com_avo_smf))

diff_avo_smf.1<-setdiff(unique(com_ebird_smf), unique((avonet_smf.1$common_name)))

# Extracting the missing scientific names
# Get scientific names for the 6 missing common names
missing_sci <- ebird_smf %>%
  filter(PRIMARY_COM_NAME %in% diff_avo_smf.1) %>%
  distinct(PRIMARY_COM_NAME, SCI_NAME, FAMILY)

print(missing_sci)



############### briged and added the scientificnames
# Taxonomic Bridge Table for non-matchables
taxonomic_bridge_smf <- data.frame(
  manual_scientific = c( "Psittiparus bakeri","Paradoxornis atrosuperciliaris","Basileuterus griseiceps" ,"Pteruthius aeralatus","Alcurus striatus","Paradoxornis gularis"),# what it's called in avo got the names by manually looking into it
  ebird_scientific = c("Paradoxornis bakeri","Suthora atrosuperciliaris","Myiothlypis griseiceps","Pteruthius aeralatus","Alcurus striatus","Paradoxornis gularis"),   # what it's called in ebird
  common_name = c("RufousHeadedParrotbill", "PaleBilledParrotbill", "GreyHeadedWarbler","WhiteBrowedShrikeBabbler","StriatedBulbul","GreyHeadedParrotbill")         # target common name
)

# 1. Prepare lookups
ebird_lookup <- ebird_smf %>%
  distinct(SCI_NAME, PRIMARY_COM_NAME) %>%
  filter(!is.na(SCI_NAME))

bridge_lookup <- taxonomic_bridge_smf %>%
  distinct(manual_scientific, ebird_scientific) %>%
  filter(!is.na(manual_scientific))

# 2. Enrich with priority matching + bridge fallback
avo_final <- avo %>%
  # Priority 1: Species2_eBird (direct)
  left_join(
    ebird_lookup %>% rename(common_p1 = PRIMARY_COM_NAME),
    by = c("Species2_eBird" = "SCI_NAME")
  ) %>%
  
  # Priority 2: eBird.species.group (direct)
  left_join(
    ebird_lookup %>% rename(common_p2 = PRIMARY_COM_NAME),
    by = c("eBird.species.group" = "SCI_NAME")
  ) %>%
  
  # Priority 3: Species1_BirdLife (direct first, then bridge)
  left_join(
    ebird_lookup %>% rename(common_p3_direct = PRIMARY_COM_NAME),
    by = c("Species1_BirdLife" = "SCI_NAME")
  ) %>%
  left_join(
    bridge_lookup %>% rename(ebird_p3 = ebird_scientific),
    by = c("Species1_BirdLife" = "manual_scientific")
  ) %>%
  left_join(
    ebird_lookup %>% rename(common_p3_bridge = PRIMARY_COM_NAME),
    by = c("ebird_p3" = "SCI_NAME")
  ) %>%
  
  # Priority 4: Species3_BirdTree (direct first, then bridge)
  left_join(
    ebird_lookup %>% rename(common_p4_direct = PRIMARY_COM_NAME),
    by = c("Species3_BirdTree" = "SCI_NAME")
  ) %>%
  left_join(
    bridge_lookup %>% rename(ebird_p4 = ebird_scientific),
    by = c("Species3_BirdTree" = "manual_scientific")
  ) %>%
  left_join(
    ebird_lookup %>% rename(common_p4_bridge = PRIMARY_COM_NAME),
    by = c("ebird_p4" = "SCI_NAME")
  ) %>%
  
  # 3. Resolve final names with full traceability
  mutate(
    common_name = coalesce(
      common_p1,           # Species2_eBird
      common_p2,           # eBird.species.group
      common_p3_direct,    # BirdLife direct
      common_p3_bridge,    # BirdLife via bridge
      common_p4_direct,    # BirdTree direct  
      common_p4_bridge     # BirdTree via bridge
    ),
    
    match_source = case_when(
      !is.na(common_p1) ~ "Species2_eBird (direct)",
      !is.na(common_p2) ~ "eBird.species.group (direct)",
      !is.na(common_p3_direct) ~ "Species1_BirdLife (direct)",
      !is.na(common_p3_bridge) ~ "Species1_BirdLife (bridged)",
      !is.na(common_p4_direct) ~ "Species3_BirdTree (direct)",
      !is.na(common_p4_bridge) ~ "Species3_BirdTree (bridged)",
      TRUE ~ "Unmatched"
    ),
    
    # Track which eBird scientific name was ultimately used
    ebird_sci_resolved = case_when(
      !is.na(common_p1) ~ Species2_eBird,
      !is.na(common_p2) ~ eBird.species.group,
      !is.na(common_p3_direct) ~ Species1_BirdLife,
      !is.na(common_p3_bridge) ~ ebird_p3,
      !is.na(common_p4_direct) ~ Species3_BirdTree,
      !is.na(common_p4_bridge) ~ ebird_p4,
      TRUE ~ NA_character_
    )
  ) %>%
  
  
# 4. Clean up
  select(-matches("^common_p[0-9]"), -matches("^ebird_p[0-9]"))

# 5. Check results
table(avo_final$match_source, useNA = "ifany")


# filtering only species which is in smf

avo_final <- avo_final[avo_final$common_name %in% smf$species,]

############ This species has Nas in trait ############
# Drop rows with any NA
avo_clean <- avo_smf %>%
  drop_na(Beak.Length_Culmen:Tail.Length)

# checking all the rows for this species 
View(avo_smf %>%
                 filter(common_name == "WhiteBrowedShrikeBabbler"))


############  count specimens per species in avo_smf dataset ############
library(dplyr)
library(ggplot2)

# 1. Basic count per species
specimen_counts <- avo_clean %>%
  filter(!is.na(common_name)) %>%  # exclude unmatched if desired
  count(common_name, name = "specimen_count") %>%
  arrange(desc(specimen_count))

# 2. Count with scientific name for reference
specimen_counts_detailed <- avo_clean %>%
  filter(!is.na(common_name)) %>%
  group_by(common_name, ebird_sci_resolved) %>%
  summarise(
    specimen_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(specimen_count))

# 3. Add count as a column to your original data (if needed for filtering)
avo_with_counts <- avo_clean %>%
  group_by(common_name) %>%
  mutate(specimens_per_species = n()) %>%
  ungroup()

# 4. Quick summary statistics
summary_stats <- specimen_counts %>%
  summarise(
    total_species = n(),
    total_specimens = sum(specimen_count),
    mean_per_species = mean(specimen_count),
    median_per_species = median(specimen_count),
    max_specimens = max(specimen_count),
    min_specimens = min(specimen_count),
    species_with_1_specimen = sum(specimen_count == 1)
  )

# 5. View distribution (optional visualization)
ggplot(specimen_counts, aes(x = specimen_count)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  scale_x_log10() +  # log scale often helps for skewed count data
  labs(
    title = "Distribution of Specimens per Species",
    x = "Number of Specimens (log scale)",
    y = "Number of Species"
  )

#quick check
# See top 10 most sampled species
head(specimen_counts, 10)

# See species with only 1 specimen (singletons)
specimen_counts %>% filter(specimen_count == 1)

# Check if any species have suspiciously high counts (data quality check)
specimen_counts %>% filter(specimen_count > 50)


# frequency of specimens per species 
View(table(avo_with_counts$common_name))
     
     
     
######## checking debudding all queries here ##############

##### adding leftover species from supply file to smf ############
wbsb <- avo_supply %>%
  filter(Species3 == "Pteruthius flaviscapis")

wbsb <- avo_supply[avo_supply$Species3 == "Pteruthius flaviscapis", ]
wbsb$common_name <- "WhiteBrowedShrikeBabbler"
wbsb %>% select(Species3, common_name)

wbsb <- wbsb %>%
  rename(Hand.wing.Index = Hand.Wing.Index)

common_cols <- intersect(colnames(avo_clean), colnames(wbsb))

wbsb_matched <- wbsb %>%
  select(all_of(common_cols))
missing_cols <- setdiff(colnames(avo_clean), colnames(wbsb_matched))

wbsb_matched[missing_cols] <- NA

avo_clean_final <- bind_rows(avo_clean, wbsb_matched)


# adding sci name to a coloumn
avo_clean_final <- avo_clean_final %>%
  mutate(
    ebird_sci_resolved = if_else(
      common_name == "WhiteBrowedShrikeBabbler",
      "Pteruthius flaviscapis",
      ebird_sci_resolved
    )
  )



# Check traits for the rescued species
avo_clean_final %>%
  filter(common_name == "WhiteBrowedShrikeBabbler")
##### adding leftover species from supply file to fc ############
wbsb <- avo_supply %>%
  filter(Species3 == "Pteruthius flaviscapis")

wbsb <- avo_supply[avo_supply$Species3 == "Pteruthius flaviscapis", ]
wbsb$common_name <- "WhiteBrowedShrikeBabbler"
wbsb %>% select(Species3, common_name)

wbsb <- wbsb %>%
  rename(Hand.wing.Index = Hand.Wing.Index)

common_cols <- intersect(colnames(avo_clean_fc), colnames(wbsb))

wbsb_matched <- wbsb %>%
  select(all_of(common_cols))
missing_cols <- setdiff(colnames(avo_clean_fc), colnames(wbsb_matched))

wbsb_matched[missing_cols] <- NA

avo_clean_fc <- bind_rows(avo_clean, wbsb_matched)


# adding sci name to a coloumn
avo_clean_fc <- avo_clean_fc %>%
  mutate(
    ebird_sci_resolved = if_else(
      common_name == "WhiteBrowedShrikeBabbler",
      "Pteruthius flaviscapis",
      ebird_sci_resolved
    )
  )



# Check traits for the rescued species
avo_clean_fc %>%
  filter(common_name == "WhiteBrowedShrikeBabbler")
##### converting avonet trait data in single species by taking mean of samples for a species #############

trait_cols <- c("Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", 
                "Beak.Depth", "Tarsus.Length", "Wing.Length", 
                "Kipps.Distance", "Secondary1", "Hand.wing.Index", 
                "Tail.Length")

# Create aggregated dataframe with means and SDs
mean_com <- avonet_com %>%
  group_by(common_name, ebird_sci_resolved) %>%
  summarise(
    # Calculate means for each trait
    across(all_of(trait_cols), 
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    # Count samples per species
    n_samples = n(),
    .groups = "drop"
  )

##### saving the files ######

#Coloumns to keep 
columns_to_keep <- c("Species1_BirdLife","Species2_eBird","eBird.species.group","Species3_BirdTree","Data.type","Source","Sex","Country_WRI","Beak.Length_Culmen",
                     "Beak.Length_Nares", "Beak.Width","Beak.Depth","Tarsus.Length","Wing.Length","Kipps.Distance","Secondary1","Hand.wing.Index","Tail.Length","Measurer"           
                     ,"common_name","match_source","ebird_sci_resolved","dataset_source")
avonet_combined<- avo_combined %>%
  select(any_of(columns_to_keep))

avonet_fc <- avonet_fc%>%
  mutate(common_name = str_replace_all(common_name, "[']", ""))  # remove all ' 
write.csv(avonet_fc, here::here("..", "..", "data_files","ready_data", "avonet_fc.csv"), row.names = FALSE)


write.csv(combined_auto, here::here("..", "..", "data_files","ready_data", "flockcomposition_19_20_24_25.csv"), row.names = FALSE)
write.csv(ebird_fc, here::here("..", "..", "data_files","ready_data", "ebird_fc.csv"), row.names = FALSE)
write.csv(avonet_smf, here::here("..", "..", "data_files","ready_data", "avonet_smf.csv"), row.names = FALSE)


write.csv(mean_com, here::here("..", "..", "data_files","ready_data", "avonet_mean_combined.csv"), row.names = FALSE)





############ merging fc and smf #########

columns_to_keep <- c("date","time","flock_id" ,"plot","species","max_ind","min_ind","call","strata","elevation","Cano_level","scientific_name","flock_id_unique")
new_smf<- smf %>%
  select(any_of(columns_to_keep))

columns_to_keep <- c("date","year","month","day","time","flock_id" ,"plot","species","max_ind","min_ind","call","strata","elevation","Cano_level","scientific_name","flock_id_unique")
new_fc<- fc %>%
  select(any_of(columns_to_keep))


# Add source identifier first
smf_tagged <- new_smf %>% mutate(source = "SMF")
fc_tagged <- new_fc %>% mutate(source = "FC")

# Use bind_rows which automatically aligns by column name
combined_auto <- bind_rows(smf_tagged, fc_tagged)

# See which columns exist in each
glimpse(combined_auto)

########### additional stuff ########
#made unique flock id for merging 
smf <- smf %>%
       mutate(flock_id_unique = paste0("2019_", flock_id))

fc <- fc %>%
      mutate(flock_id_unique = paste0("2024_", flock_id))
