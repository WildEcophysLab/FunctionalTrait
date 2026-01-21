# Making changes in Sarthak Data code and as well making combined data set 

##### Loading all the packages ####
library(tidyverse)
library(here)
library(dplyr)
library(fuzzyjoin)
library(dplyr)
library(readr)
library(stringr)

##### File reading #########
fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
smf<-read.csv(here("..","..","data_files", "sarthak_2019_20_mf_data_updated.csv"))
usmf<-read.csv(here("..","..","data_files", "unique_smf.csv")) # this file has all the unique species names with scientific names 
#reading AVO-NET data here 
avo<-read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))


##### renaming coloumn as per 2024 data sheet so all the notation remain same ############
smf<-smf%>%
 rename(max_ind=num.low, min_ind=num.up)

##### changing the spellings of names ##############
  smf$species <- gsub("gray", "Grey", smf$species, ignore.case = TRUE)

##### adding additional value in flock id so it when two files are merged it can be compareable ##############
fc.c<-fc %>%
    mutate(flock_id = paste0(flock_id, "sk"))
smf.c<-smf %>%
  mutate(flock_id = paste0(flock_id, "sm"))


##### adding the elevation values and notation for Canopy level as C for cannopy and MU for both understory and midstory and vice versa#####
  smf <- smf %>%
  mutate(
    elevation = case_when(
      plot == "tragopanda lake" ~ 2800,
      plot == "Eaglenest pass" ~ 2800,
      plot == "Bomphu" ~ 1500,
      plot == "Chaku" ~ 2400,
      plot == "khelong" ~ 800,
      plot == "Khelong" ~ 800,
      plot == "top-sesni" ~ 1600,
      plot == "down-sesni" ~ 1200,
      TRUE ~ NA_real_
    ),
    canopy_level = case_when(
      strata == "top" ~ "C",
      strata == "bottom" ~ "MU",
      TRUE ~ NA_character_
    )
  )
  
  
  
  fc<-fc %>%
    mutate(
      strata = case_when(
        canopy_level == "M" ~ "MU",
        canopy_level == "U" ~ "MU",
        canopy_level == "C" ~ "C",
        TRUE ~ NA_character_
      )
    )

  
##### Optional:checking which of the coloumn have NAs ###########
smf %>%
   filter(is.na(strata))

##### converting the name in pascal caseview###########
  # Step 1: Replace hyphens with spaces
  smf$species <- gsub("-", " ", smf$species)
  # The gsub() function is a powerful base R function for finding and replacing patterns in strings. 
  #This line finds every hyphen ("-") replaces it with a single space (" ").
  
  # Step 2: Apply tools::toTitleCase() to capitalize each word
  smf$species <- tools::toTitleCase(smf$species)
  # tools package is base R, It capitalizes the first letter of each word in a string, effectively converting it to Pascal Case (Title Case)
  
  # Step 3: Remove all remaining spaces to create a single word
  smf$species <- gsub(" ", "", smf$species)
  #this line finds every space (" ") in the modified strings and replaces them with nothing (""),
  #effectively joining all the words together into a single Pascal Case string


##### Fuzzy matching to see any errors in name ##########
  # Assuming your columns are named 'Species', ensure they are of type character
  fc$species <- as.character(fc$species)
  smf$species <- as.character(smf$species)
  
  # Perform the fuzzy join
  # Use 'jw' method for Jaro-Winkler distance
  # Adjust max_dist as needed. A value between 0.1 and 0.2 is common for a tight match.
  temp_df <- stringdist_join(
    fc,
    smf,
    by = "species",
    mode = "left",
    method = "jw",
    max_dist = 0.2,
    distance_col = "distance_score"
  )
  
  # Select only the species columns from both data frames and the distance score
  matched_df <- temp_df %>%
    select(fc_species = species.x, smf_species = species.y, distance_score)
  
  filtered_df <- matched_df %>%
    filter(distance_score > 0)

##### Unique species in both fiels #######  
  unique_smf<-setdiff(smf$species, fc$species)
##### saving the updated files ###########
combined.fc <- bind_rows(smf.c,fc.c)
  
write.csv(smf, here::here("..", "..", "data_files", "sarthak_2019_20_mf_data_updated.csv"), row.names = FALSE)
write.csv(fc, here::here("..", "..", "data_files", "flock_composition_data_cleaned.csv"), row.names = FALSE)
write.csv(combined.fc, here::here("..", "..", "data_files", "flock_composition_data_19_20_24_25.combined.csv"), row.names = FALSE)


























##### adding scientific names to smf file ####
 # sanity check
# unique species in each file
length(unique(smf$species))
length(unique(fc$species))
length(unique(usmf$species))

# check overlaps
sum(smf$species %in% fc$species)
sum(smf$species %in% usmf$species)

# master table, combing both files
lookup <- rbind(
  fc[, c("species", "scientific_name")],
  usmf[, c("species", "scientific_name")]
)
# removing the duplicates
lookup <- rbind(
  fc[, c("species", "scientific_name")],
  usmf[, c("species", "scientific_name")]
)
# matching the scientific names into smf
smf$scientific_name <- lookup$scientific_name[
  match(smf$species, lookup$species)
]
# rechecking 
sum(!is.na(smf$scientific_name))
sum(is.na(smf$scientific_name))
 # seeing missing one
unique(smf$species[is.na(smf$scientific_name)])







##### removing the flocks which have issues with names of canopy height ####

#finding the flock id of those 
bad_flocks <- unique(
  smf$flock_id[is.na(smf$scientific_name)]
)

# removing the entire flock
smf <- smf[!(smf$flock_id %in% bad_flocks), ] 
# recheck with this "sum(is.na(smf$scientific_name))"

##### Step-2:Removing the unnecessary columns from avo Net #####
  columns_to_keep <- c("common_name", "Avibase.ID", "Species1_BirdLife", "Species2_eBird", "eBird.species.group", "Species3_BirdTree", "Data.type", "Source",
                       "Specimen.number", "Sex", "Age", "Locality", "Country_WRI", "Country", "Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", "Beak.Depth", 
                       "Tarsus.Length", "Wing.Length", "Kipps.Distance", "Secondary1", "Hand.wing.Index", "Tail.Length", "Measurer", "Protocol", "Publication")
  avo<- avo %>%
    select(any_of(columns_to_keep))
  # (filename$columnname <-NULL  ) deletes the column from the dataframe.








{
  
  # Create the lookup table from 'fc'
  # This ensures we have a unique mapping of scientific name to species name.
  species_lookup <- smf %>%
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