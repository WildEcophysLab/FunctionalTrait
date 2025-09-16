library(tidyverse)
library(here)

{
  ebird_taxa<-read.csv(here("..","..","data_files", "eBird_taxonomy_v2024.csv"))
  ebird_taxa <- ebird_taxa %>%
    mutate(PRIMARY_COM_NAME = str_replace_all(PRIMARY_COM_NAME, "[- ]", " "),  # replace - and space with space
           PRIMARY_COM_NAME = str_to_title(PRIMARY_COM_NAME),                 # title case
           PRIMARY_COM_NAME = str_replace_all(PRIMARY_COM_NAME, " ", ""))     # remove all spaces (PascalCase)
  
  #<reading the flock composition file
  fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
  #reading the BirdNET analyzer data
  species_df<-read.csv(here("..","..","data_files", "species_list_cleaned.csv"))
  species_df<-species_df %>%
    mutate(common_name = str_replace_all(common_name, "[- ]", " "),  # replace - and space with space
           common_name = str_to_title(common_name),                 # title case
           common_name = str_replace_all(common_name, " ", ""))     # remove all spaces (PascalCase)
  #reading AVO-NET data here 
  avo<-read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))
}#Step-1:loading all required  files and also changing the common name to pascal case so it can be easy to compare between files

{
  columns_to_keep <- c("common_name", "Avibase.ID", "Species1_BirdLife", "Species2_eBird", "eBird.species.group", "Species3_BirdTree", "Data.type", "Source",
                       "Specimen.number", "Sex", "Age", "Locality", "Country_WRI", "Country", "Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", "Beak.Depth", 
                       "Tarsus.Length", "Wing.Length", "Kipps.Distance", "Secondary1", "Hand.wing.Index", "Tail.Length", "Measurer", "Protocol", "Publication")
  avo<- avo %>%
    select(any_of(columns_to_keep))
  # (filename$columnname <-NULL  ) deletes the column from the dataframe.
}#Step-2:Removing the unnecessary columns from avo Net 

{
  # Create a vector of species scientific names
  target_species <- unique(fc$species)
  
  species_df <- species_df %>%
    filter(
      common_name %in% target_species
    )
#Step-3:comparing species_df and fc species name to filter out extra rows of species names

 #dataframe join/relation join
  # Left Join (or Left Merge), A left join combines two datasets based on a common key , and adds matching columns from the second dataset
  # to the first. If no match is found, the original data is kept, and the new columns will be NA
  
  fc <- fc %>%
    left_join(species_df %>% 
                select(common_name, scientific_name),
              by = c("species" = "common_name"))
  
}#Step-3 & 4: Comparing species_df and fc species name to filter out extra rows of species names  and adding a new column as scientific name into fc by comparing species name/common name and assinging them scientific name

{
  fc_missing_sn <- fc %>%
    filter(is.na(scientific_name)) %>%
    distinct(species)  # optional: list each unmatched species only once
  
  print(fc_missing_sn)
  
}#Step-5:Checking which species does not have scientific name in flock composition

{
  # Step 1: Define your manual fix mapping — use corrected common names
  manual_fixes <- data.frame(
    common_name = c(
      "GreyHeadedParrotbill", "GreyHeadedCanaryFlycatcher", "GreyHeadedWoodpecker", 
      "Whitstler'sWarbler", "GreyHoodedWarbler", "YellowBelliedFantail", 
      "GreyCheekedWarbler", "GreyThroatedBabbler", "RufousVentedLaughingthrush", 
      "WhiteBrowedShrikeBabbler", "GreyCheckedWarbler", "GrayHoodedWarbler", 
      "RufousFrontedBabbler", "WhiteBreastedParrotbill", "GreyChinnedMinivet", 
      "WhiteNapedYuhina", "BlackCrownedScimitarBabbler", "BeautifulNuthatch", 
      "YellowBelliedFairyFantail", "GreySidedLaughingthrush", "BlueWingedLaughingthrush", 
      "RustyFlankedTreecreeper", "GreyBelliedTesia", "CrimsonNapedWoodpecker", 
      "ShortBilledMinivet", "StripedThroatedYuhina", "BrownThroatedFulvetta", 
      "YellowBelliedFlowerpecker", "GreyCrestedTit", "StreakThroatedBarwing", 
      "RufousNapedTit"
    ),
    scientific_name = c(
      "Psittiparus gularis", "Culicicapa ceylonensis", "Picus canus", 
      "Phylloscopus whistleri", "Phylloscopus xanthoschistos", "Chelidorhynx hypoxanthus", 
      "Phylloscopus poliogenys", "Stachyris nigriceps", "Pterorhinus gularis", 
      "Pteruthius aeralatus", "Phylloscopus poliogenys", "Phylloscopus xanthoschistos", 
      "Cyanoderma rufifrons", "Paradoxornis ruficeps", "Pericrocotus solaris", 
      "Yuhina bakeri", "Pomatorhinus ferruginosus", "Sitta formosa", 
      "Chelidorhynx hypoxanthus", "Pterorhinus caerulatus", "Trochalopteron squamatum", 
      "Certhia nipalensis", "Tesia cyaniventer", "Dryobates cathpharius", 
      "Pericrocotus brevirostris", "Yuhina gularis", "Fulvetta ludlowi", 
      "Dicaeum melanoxanthum", "Lophophanes dichrous", "Actinodura waldeni", 
      "Periparus rufonuchalis"
    ),
    stringsAsFactors = FALSE
  )
 
  fc <- fc %>%
    left_join(manual_fixes, by = c("species" = "common_name")) %>%
    mutate(scientific_name.y = ifelse(is.na(scientific_name.y), scientific_name, scientific_name.y)) %>%
    select(-scientific_name)
  
  
  # > fc$species[fc$species %in% manual_fixes$common_name] %>% unique() [ find species in your dataset that are present in manual_fixes]
  
}#Step-6:manually adding scientific names to common names in FC 

{
  fc <- fc %>%
    mutate(species = case_when(species == "BlackThroatedFulvetta" ~ "BlackFacedFulvetta",
                               TRUE ~ species))
  
  write.csv(fc, here("..","..","data_files", "flock_composition_data_cleaned.csv"), row.names = FALSE)
  
}#Step-7:Fixing the error and wrong species names manually

{
  # Join fc with ebird_taxa using common name
  species_names <- fc %>%
    select(species, scientific_name) %>%
    distinct() %>%
    left_join(ebird_taxa %>% select(PRIMARY_COM_NAME, SCI_NAME),
              by = c("species" = "PRIMARY_COM_NAME"))
  
  # View it nicely
  print(species_names)  # To see all rows if needed
  
}#Step-8:comparing flock composition and e-bird_taxa to see which scientific name is different 

#{
  avo <- avo %>%
    filter(!is.na(common_name))
  
}#Step-9: removed the NA rows in common name

{
  
  # Create the lookup table from 'fc'
  # This ensures we have a unique mapping of scientific name to species name.
  species_lookup <- fc %>%
    select(scientific_name, species) %>%
    distinct(scientific_name, .keep_all = TRUE)
  
  # Pivot the 'avo' data to a longer format
  avo_long <- avo %>%
    # Use 'Avibase.ID' as the unique identifier for each row
    pivot_longer(
      cols = c(Species1_BirdLife, Species2_eBird, eBird.species.group, Species3_BirdTree),
      names_to = "source_column",
      values_to = "scientific_name_match",
      values_drop_na = TRUE  # This removes rows where the species name is NA
    ) %>%
    # Clean up any leading/trailing whitespace
    mutate(scientific_name_match = trimws(scientific_name_match))
  
  # Join the longer data with the lookup table
  avo_joined <- avo_long %>%
    left_join(species_lookup, by = c("scientific_name_match" = "scientific_name"))
  
  # Consolidate the results back to the original 'avo' structure
  # We group by the unique ID and take the first non-NA species match found.
  avo_final <- avo_joined %>%
    group_by(Avibase.ID) %>%
    # Get the first non-NA species name for each Avibase.ID
    summarise(
      species = first(na.omit(species)), 
      .groups = "drop"
    )
  
  # Add the new 'species' column to the original 'avo' dataframe
  avo_with_species <- avo %>%
    left_join(avo_final, by = "Avibase.ID")
  
  avo_with_species <- avo_with_species %>%
      filter(!is.na(species))
  
  write.csv(avo_with_species, here::here("..", "..", "data_files", "avo_with_species.csv"), row.names = FALSE)
  
  # The 'avo_with_species' dataframe now has the new column.
  
}#Step-10: added common name into avo datasheet

{
  
  # Find common names in avo_clean not present in fc$species
  missing_in_fc <- avo_with_species %>%
    select(species) %>%
    distinct() %>%
    anti_join(fc %>% select(species) %>% distinct(), 
              by = c("species" = "species"))
  
  # View missing common names
  print(missing_in_fc)
  
}#Step-11: Checking which species is not there in avo file after adding the names and filtering it out

{
  library(dplyr)
  
  # Step 1: Get frequency tables
  avo_counts <- table(avo_with_species$species)
  fc_counts  <- table(fc$species)
  
  # Step 2: Combine into a single data frame (keep all species from both)
  combined <- full_join(
    as.data.frame(avo_counts),
    as.data.frame(fc_counts),
    by = "Var1",
    suffix = c("_avo", "_fc")
  )
  
  # Step 3: Rename columns
  names(combined)[1] <- "species"
  names(combined)[2] <- "occurrence_avo"
  names(combined)[3] <- "occurrence_fc"
  
  # Step 4: Add a mismatch flag (TRUE if counts differ or one is missing)
  combined <- combined %>%
    mutate(
      mismatch_flag = occurrence_avo != occurrence_fc |
        is.na(occurrence_fc) |
        is.na(occurrence_avo)
    )
  
  # Step 5: Join with fc to bring in scientific_name
  # and join with avo_with_species for other columns
  combined_full <- combined %>%
    left_join(
      fc %>% select(species, scientific_name),
      by = "species"
    ) %>%
    left_join(
      avo_with_species %>% 
        select(species, Species1_BirdLife, Species2_eBird,
               eBird.species.group, Species3_BirdTree),
      by = "species"
    ) %>%
    distinct() %>%
    relocate(scientific_name, .before = species)  # move scientific_name to start
  
  # Step 6: Save mismatches separately (optional)
  mismatch_full <- combined_full %>%
    filter(mismatch_flag)
  
  # Step 7: Print both
  print(combined_full)   # all species
  print(mismatch_full)   # only mismatches
  
  # Step 8: Save files (optional)
  # write.csv(combined_full, "all_species_occurrences.csv", row.names = FALSE)
  # write.csv(mismatch_full, "mismatch_species_occurrences.csv", row.names = FALSE)
  

  write.csv(combined_full, here::here("..", "..", "data_files", "avo_fc_scientific_names.csv"), row.names = FALSE)
  
  #OR
  #searching only[ species from fc were never matched or used in your avo]
  
  # setdiff(fc$species, avo$species)
  
}#Step-12:Checking Which species is repeated and how much repeated in frequency



# 
# full_list <- tibble(species = union(unique(fc$species), unique(mismatch_full$species))) %>%
#   mutate(in_fc = species %in% unique(fc$species),
#          in_mismatch = species %in% unique(mismatch_full$species))
# 
# full_list %>% filter(!in_fc | !in_mismatch)
# 
# 
# 
#  all rows from avo_with_species where the species column is "AshyDrongo"
# View(
#   +     avo_with_species %>% 
#     +         filter(species == "AshyDrongo")
#   + )