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

# ALPHA DIVERSITY INDICES

# load the raw abundance data

andamans_abund <- read_csv("andamans_data.csv")

# Save site info in temp object

temp <- andamans_abund$Site

# Remove "Site" column (since it contains non-integers) 

andamans_abund$Site <- NULL

# Calculate local diversity indices

N <- rowSums(andamans_abund)
S <- rowSums(andamans_abund>0)

PIE <- rarefy(andamans_abund, 2) -1
S_pie <- 1/(1-PIE)

div_local <- cbind(N,S,PIE, S_pie)
div_local <- as.data.frame(div_local)


# add Site info to diversity dataframe

div_local$Site <- Site

# Calculate average diversity values per Site 
# that is : diversity values averaged across the number of plots or quadrats

div_alpha <- div_local %>%
group_by(Site) %>%
summarise_all(mean)

# ----------------------------------------------------------------------------------

# GAMMA DIVERSITY INDICES

# load the raw abundance data

andamans_abund <- read_csv("andamans_data.csv")

# Get summed up values of abundance data per site

andamans_abund_sum <- andamans_abund %>%
group_by(Site) %>%
summarise_all(sum)

# Save site info in temp object

Site2 <- andamans_abund_sum$Site

# Remove "Site" column (since it contains non-integers) 

andamans_abund_sum$Site <- NULL

# Calculate gamma diversity indices

N_gamma <- rowSums(andamans_abund_sum)
S_gamma <- rowSums(andamans_abund_sum>0)

PIE_gamma <- rarefy(andamans_abund_sum, 2) -1
S_pie_gamma <- 1/(1-PIE)

div_gamma <- cbind(N_gamma,S_gamma,PIE_gamma, S_pie_gamma)
div_gamma <- as.data.frame(div_gamma)

# get Site info from div_alpha

div_gamma$Site <- Site 

# merge alpha and gamma diversity indices into one dataframe

diversity_indices <- merge(div_alpha, div_gamma, by = "Site")

# ----------------------------------------------------------------------------------

# CHAO RICHNESS 
# Calculating S_chao values from summed up abundance data
# S_chao is referred to as S_total in the manuscript 

# Transpose data to fit iNEXT format
t_andamans_abund_sum<- t(andamans_abund_sum)
# Convert into a dataframe
t_andamans_abund_sum <- as.data.frame(t_andamans_abund_sum)
# Calculate chao using the ChaoRichness function in iNEXT
chao_values <- ChaoRichness(t_andamans_abund_sum, datatype = "abundance")

# Add S_chao/S_total values to the "diversity_indices" dataframe 

diversity_indices$S_total <- chao_values$Estimator








