
####################################################################################
# Author : Leana Gooriah
# Email : leana_devi.gooriah@idiv.de
####################################################################################

# Description : This script can be used to calculate the different diversity indices 
# Such as : species richness (S), Effective Number of Species of PIE (S_pie) 
# at two different scales (alpha and gamma).

####################################################################################

# LOAD THE NECESSARY R LIBRARIES

library(vegan)
library(dplyr)
library(tidyr)
library(iNEXT)
library(readr)

# ----------------------------------------------------------------------------------

# Set path and directories

path1 <- ("~/Desktop/ISAR_DATA/")
data_path <- paste(path1, "datasets/", sep = "")
div_path <- paste(path1, "diversity_indices/", sep = "")

# Read all file names
filenames <- list.files(data_path, pattern="*.csv*", full.names = F)

# Get list of study ids
filename_roots <- gsub("_data.csv","",filenames) # remove _data from filenames

study_ids <- unique(filename_roots)
study_ids

# Prepare output data
div_list <- list()


# ALPHA & GAMMA DIVERSITY INDICES 

# Loop over all studies

for (i in 1:3){
  
  data_file <- paste(data_path, study_ids[i], "_data.csv", sep ="")
  print(data_file)
  
  dat1 <- read.csv(data_file, header = TRUE, sep =",", row.names = 1)
  
  N_local <- rowSums(dat1)
  S_local <- rowSums(dat1>0)
  PIE_local <- rarefy(dat1, 2) -1
  S_pie_local <- 1/(1-PIE_local)
  
  div_local <- cbind(N_local, S_local, PIE_local, S_pie_local)
  div_local <- as.data.frame(div_local)
  
  
  
  # Get average diversity values per Site 
  
  average_div <- div_local %>%
    group_by(Site) %>%
    summarise_all(mean)
  
  # Sum up the raw abundance data
  
  dat1 <- read.csv(data_file, header = TRUE, sep =",", row.names = 1)
  
  data_sum <- dat1 %>%
  group_by(Site) %>%
  summarise_all(sum)
  
  
  # Calculate gamma diversity indices
  
  N_gamma <- rowSums(data_sum)
  S_gamma <- rowSums(data_sum>0)
  PIE_gamma <- rarefy(data_sum, 2) -1
  S_pie_gamma <- 1/(1-PIE_gamma)
  
  div_gamma <- cbind(N_gamma, S_gamma, PIE_gamma, S_pie_gamma)
  div_gamma <- as.data.frame(div_gamma)  
  
  # Merge alpha and gamma diversity indices into one dataframe
  
  diversity_indices <- merge(average_div, div_gamma, by = "Site")
  
  # CHAO RICHNESS
  # Calculating S_chao values from summed up abundance data
  # S_chao is referred to as S_total in the manuscript
  
  # Transpose data to fit iNEXT format requirements
  t_data_sum <- t(data_sum)
  # Convert into a dataframe
  t_data_sum <- as.data.frame(t_data_sum)
  # Calculate chao using the ChaoRichness function in iNEXT
  chao_values <- ChaoRichness(t_data_sum, datatype = "abundance")
  
  # Add S_chao/S_total values to the "diversity_indices" dataframe
  
  diversity_indices$S_total <- chao_values$Estimator


  div_list[[i]] <- diversity_indices

}

# Save the individual diversity csv files in the "diversity_indices" folder
write.csv2(div_list[[1]], "~/Desktop/ISAR_DATA/diversity_indices/andamans.csv")
write.csv2(div_list[[2]], "~/Desktop/ISAR_DATA/diversity_indices/glades.csv")
write.csv2(div_list[[3]], "~/Desktop/ISAR_DATA/diversity_indices/fragments.csv")



