library(tidyverse)
library(here)
# library(dplyr)
# library(tidyr)  already included in tidyverse
# library(stringr)
# library(googledrive)
# drive_auth()



{#<Species list file from Bird Net analyzer for fetching scientific name and common name
  species_lines <- readLines("..//..//data_files//species_list.txt")
  #abc <- readLines(here("..", "..", "data_files", "species_list.txt"))
  
  #<reading the flock composition file
  fc<-read.csv(here("..","..","data_files", "flock_composition_data_cleaned.csv"))
  
  #<Reading AVO net raw species files to do comparison and pull out only needed species list numbers.
  # Access AVONET data from outside the R Project, here function used to make it easily accessible for anyone, directory is 25_07_cleaning for R proj
  # avo_net<-read.csv(file.path("..//..//..//TorporPhylogeny//ELE//ELEData//TraitData//AVONET_Raw_Data.csv"))  
  # < the above line also worked
  avo_net<- read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))
  
  #<Species with trait data with filter of country India from AVO net trait data 
  fai<-read.csv(here("..","..","data_files", "AVONET_filtered_species.csv"))
  
  #<Species with trait data without any filter for country=India from AVO net trait data 
  fao<-read.csv(here("..","..","data_files", "AVONET_filtered_species_overall.csv"))
  
  #< this csv is created by spliting common name and species name from txt file and making into a csv to use
  sp_list<- read.csv(here("..","..","data_files", "species_list_cleaned.csv"))
  sp_list <- sp_list %>%
    mutate(common_name = str_replace_all(common_name, "[- ]", " "),  # replace - and space with space
           common_name = str_to_title(common_name),                 # title case
           common_name = str_replace_all(common_name, " ", ""))     # remove all spaces (PascalCase)
  
}#Reading ready files here from G-Drive at starting only so no to read twice or make confusion, also code lengthy 

{
  # Split by underscore
  species_split <- strsplit(species_lines, "_")
  
  # Convert to data frame
  species_df <- do.call(rbind, lapply(species_split, function(x) {
    data.frame(scientific_name = x[1], common_name = x[2], stringsAsFactors = FALSE)
  }))
  
  head(species_df)
  # dir.create(here("..", "..", "data_files"), showWarnings = FALSE, recursive = TRUE)
  write.csv(species_df, here("..", "..", "data_files", "species_list_cleaned.csv"), row.names = FALSE)
}#<Spliting species and scientific names, also made csv files from the .txt data set

{
  # Access AVONET data from outside the R Project, here function used to make it easily accessible for anyone, directory is 25_07_cleaning for R proj
  # avo_net2 <- read.csv(here("..", "..","..", "TorporPhylogeny", "ELE", "ELEData", "TraitData", "AVONET_Raw_Data.csv"))
  # < the above line worked
  
  
  #  reading the files where  commmon names and scientific names
  # <   ".." moves up one level
  # <   So "..", ".." takes you from scripts/ → Functional_trait/ → Lab Projects/
  
  # Create a vector of species scientific names
  target_species <- sp_list$scientific_name
  
  # Filter avo_net for rows that:
  # (1) Country_WRI is India
  # (2) Any of the 4 species columns match the names in species_df
  filtered_avo_net <- avo_net %>%
    filter(Country_WRI == "India" & (
      Species1_BirdLife %in% target_species |
        Species2_eBird %in% target_species |
        eBird.species.group %in% target_species |
        Species3_BirdTree %in% target_species
    ))
  
  # Add common_name by matching species names to any of the 4 columns
  filtered_avo_net <- filtered_avo_net %>%
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
    left_join(species_df, by = c("match_species" = "scientific_name")) %>%
    relocate(common_name, .before = everything()) %>%
    ungroup()
  
  filtered_avo_net <- filtered_avo_net %>%
    mutate(common_name = str_replace_all(common_name, "[- ]", " "),  # replace - and space with space
           common_name = str_to_title(common_name),                 # title case
           common_name = str_replace_all(common_name, " ", ""))     # remove all spaces (PascalCase)
  
  # Check result
  # print(nrow(filtered_avo_net))  # number of rows that matched
  
  # Save to CSV
  write.csv(filtered_avo_net, here::here("..", "..","data_files", "AVONET_filtered_species.csv"), row.names = FALSE)
}#<Filterd out the species from avo_net trit data, by COUNTRY india and species found in studysite(sp_list).filtered_avo_net

{
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
  
}#<made a seprate list without country filter just by species name without any filter for country, filtered_avo_net_2

{
  #taking unique species out from flock composition file
  fc_u <- unique(fc$species)
  
  #filtering flock composition species from filtered avonet data only for india specimens
  fc_avo<- fai %>%
    filter(common_name %in% fc_u)
  
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
} #<Checking which species is not in AVO net with compare to flock composition

{
  # Add scientific name using left_join
  leftsp_df <- leftsp_df %>%
    left_join(sp_list, by = "common_name")
  
}
