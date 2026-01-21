library(tidyverse)
library(here)
library(dplyr)
# library(tidyr)  already included in tidyverse
# library(stringr)

##### Reading ready files here from G-Drive at starting only so no to read twice or make confusion, also code lengthy #############
  #<Species list file from Bird Net analyzer for fetching scientific name and common name
  species_lines <- readLines("..//..//data_files//species_list.txt")
  #abc <- readLines(here("..", "..", "data_files", "file.txt"))
  
  #<reading the flock composition file
  fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
  smf<-read.csv(here("..","..","data_files", "sarthak_2019_20_mf_data_updated.csv"))
  
  #<Reading AVO net raw species files to do comparison and pull out only needed species list numbers.
  # Access AVONET data from outside the R Project, here function used to make it easily accessible for anyone, directory is 25_07_cleaning for R proj
  # avo_net<-read.csv(file.path("..//..//..//TorporPhylogeny//ELE//ELEData//TraitData//AVONET_Raw_Data.csv"))  
  # < the above line also worked
  avo_net<- read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))
  
  #<Species with trait data without any filter for country=India from AVO net trait data 
  fao<-read.csv(here("..","..","data_files", "AVONET_filtered_species_overall.csv"))
  fao<- read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))
  

##### Spliting species and scientific names, also made csv files from the .txt data set ####
  # Split by underscore
  species_split <- strsplit(species_lines, "_")
  
  # Convert to data frame
  species_df <- do.call(rbind, lapply(species_split, function(x) {
    data.frame(scientific_name = x[1], common_name = x[2], stringsAsFactors = FALSE)
  }))
  
  head(species_df)
  # dir.create(here("..", "..", "data_files"), showWarnings = FALSE, recursive = TRUE)
  write.csv(species_df, here("..", "..", "data_files", "species_list_cleaned.csv"), row.names = FALSE)
  
 #< this csv is created by spliting common name and species name from txt file and making into a csv to use
  sp_list<- read.csv(here("..","..","data_files", "species_list_cleaned.csv"))
  # making all of the names in pascal case
  sp_list <- sp_list %>%
    mutate(common_name = str_replace_all(common_name, "[- ]", " "),  # replace - and space with space
           common_name = str_to_title(common_name),                 # title case
           common_name = str_replace_all(common_name, " ", ""))     # remove all spaces (PascalCase)

#### made a seprate coloum just by common species name without any filter for country, filtered_avo_net_2 ####
  
  # i tried this snipet to see how many speciemens or data is from India.
  # Filter avo_net for rows that:
#   # (1) Country_WRI is India
#   # (2) Any of the 4 species columns match the names in species_df
#   filtered_avo_net <- avo_net %>%
#     filter(Country_WRI == "India" & (
#       Species1_BirdLife %in% target_species |
#         Species2_eBird %in% target_species |
#         eBird.species.group %in% target_species |
#         Species3_BirdTree %in% target_species
#     ))
#   
  
  
  # Filter avo_net for rows without India:
  # (1) Any of the 4 species columns match the names in sp_list
  filtered_avo_net_2 <- avo_net %>%
    filter(
      Species1_BirdLife %in% target_species |
        Species2_eBird %in% target_species |
        eBird.species.group %in% target_species |
        Species3_BirdTree %in% target_species)
  
  # Add common_name by matching species names to any of the 34 columns
  filtered_avo_net_2 <- filtered_avo_net_2 %>%
    rowwise() %>%
    mutate(
      match_species = case_when(
        Species1_BirdLife %in% target_species ~ Species1_BirdLife,
        Species2_eBird %in% target_species ~ Species2_eBird,
        eBird.species.group %in% target_species ~ eBird.species.group,
        Species3_BirdTree %in% target_species ~ Species3_BirdTree,
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(sp_list, by = c("match_species" = "scientific_name")) %>%
    relocate(common_name, .before = everything()) %>%
    ungroup()
  
  filtered_avo_net_2 <- filtered_avo_net_2 %>%
    mutate(common_name = str_replace_all(common_name, "[- ]", " "),  # replace - and space with space
           common_name = str_to_title(common_name),                 # title case
           common_name = str_replace_all(common_name, " ", ""))     # remove all spaces (PascalCase)
  
  # Check result
  # print(nrow(filtered_avo_net))  # number of rows that matched
  # Save to CSV
  write.csv(filtered_avo_net_2, here::here("..", "..","data_files", "AVONET_filtered_species_overall.csv"), row.names = FALSE)
  


##### Checking which species is not in AVO net with compare to flock composition ####
  #taking unique species out from flock composition file
  fc_u <- unique(smf_df$species)
  
  #filtering flock composition species from filtered avonet data overall
  fc_avoall<- fao %>%
    filter(common_name %in% fc_u)
  
  # Extract only the species names from AVONET
  onsp<- unique(fc_avoall$common_name)
  
  # Find species in flock composition that are NOT in AVONET
  leftsp<-setdiff(fc_u,onsp)
  #< setdiff() works only on vectors
  #< You must extract a single column (like common_name) before using it
  #< Comparing data frame vs vector causes wrong results or errors  
  
  # View or save the result
  leftsp_df <- data.frame(common_name = leftsp)

  
####checking which species is missing from BirdNET analyzer and also some common names are not from india, so just to check my mistakes and correct it ####
  fc_species <- unique(fc$species)
  leftsp_species <- unique(leftsp_df$common_name)
  
  present_in_fc_not_missing <- setdiff(fc_species, leftsp_species)
  
  
  write.csv(final_data,here::here("..", "..", "data_files", "unique_smf.csv"), row.names = TRUE)
  
  
  
  unique_smf<-setdiff(smf$species, fc$species)  
  
  
  
  # To get unique IDs which has NAs
  smf %>%
    filter(is.na(strata)) %>%
    distinct(flock_id) 
  
  # Convert to data frame and name the column 'species'
  smf_df <- data.frame(species = unique_smf)
  
  # View it
  head(smf_df)
  
  
  
##Option 1: Using dplyr (Recommended)
#  This is generally the cleanest approach.
  
 # left_join: Keeps all rows from your smf_df (even if a match isn't found).

#inner_join: Keeps only the rows where a match matches in both files.  
  library(dplyr)
  
  # Perform the join
  final_df <- smf_df %>%
    left_join(sp_list, by = c("species" = "common_name")) %>%
    select(species, scientific_name) # Select only the columns you want
  
  # View the result
  head(final_df)
  
###base R function
  # Merge the two data frames
  merged_data <- merge(smf_df, sp_list, by.x = "species", by.y = "common_name", all.x = TRUE)
  
  # Select just the scientific name column if that is all you need
  final_data <- merged_data[, c("species", "scientific_name")]
  
  head(final_data)