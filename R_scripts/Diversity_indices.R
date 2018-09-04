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

lizards_abund <- read_csv("lizards_andamans - abund.csv")

# Remove "Site" column (since it contains non-integers) 

lizards_abund$Site <- NULL

# Calculate local diversity indices

N <- rowSums(lizards_abund)
S <- rowSums(lizards_abund>0)

PIE <- rarefy(lizards_abund, 2) -1
S_pie <- 1/(1-PIE)

div_local <- cbind(N,S,PIE, S_pie)
div_local <- as.data.frame(div_local)


# re-load the raw abundance csv file to get Site info

lizards_abund <- read_csv("lizards_andamans - abund.csv")

div_local$Site <- lizards_abund$Site

# Calculate average diversity values per Site 
# that is : diversity values averaged across the number of plots or quadrats
div_alpha <- div_local %>%
group_by(Site) %>%
summarise_all(mean)

# ----------------------------------------------------------------------------------

# GAMMA DIVERSITY INDICES

# load the raw abundance data

lizards_abund <- read_csv("lizards_andamans - abund.csv")

# Get summed up values of abundance data per site

lizards_abund_sum <- lizards_abund %>%
group_by(Site) %>%
summarise_all(sum)

# Remove "Site" column (since it contains non-integers) 

lizards_abund_sum$Site <- NULL

# Calculate gamma diversity indices

N_gamma <- rowSums(lizards_abund_sum)
S_gamma <- rowSums(lizards_abund_sum>0)

PIE_gamma <- rarefy(lizards_abund_sum, 2) -1
S_pie_gamma <- 1/(1-PIE)

div_gamma <- cbind(N_gamma,S_gamma,PIE_gamma, S_pie_gamma)
div_gamma <- as.data.frame(div_gamma)

# get Site info from div_alpha

div_gamma$Site <- div_alpha$Site 

# merge alpha and gamma diversity indices into one dataframe

diversity_indices <- merge(div_alpha, div_gamma, by = "Site")






