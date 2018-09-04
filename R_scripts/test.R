#Calculating local diversity indices alpha S_pie, species richness (S) and number of individuals (N)

# Read csv file

lizards_abund <- read_csv("lizards_andamans - abund.csv")


#Load R packages
library(vegan)
library(dplyr)
library(tidyr)
library(iNEXT)
library(readr)

# Calculate local diversity indices

lizards_abund$Site <- NULL

N <- rowSums(lizards_abund)
S <- rowSums(lizards_abund>0)

PIE <- rarefy(lizards_abund, 2) -1
S_pie <- 1/(1-PIE)

div_local <- cbind(N,S,PIE, S_pie)
div_local <- as.data.frame(div_local)


#re-load csv to get Site info
lizards_abund <- read_csv("lizards_andamans - abund.csv")

div_local$Site <- lizards_abund$Site


#Average N per island
Average_N_island <- div_local$N

###### Calculating alpha Sn values #####

#Transpose data to fit iNEXT data format requirement
t_lizards_abund<- t(lizards_abund)
t_lizards_abund <- as.data.frame(t_lizards_abund)


# Get average N values from which corresponding Sn values will be interpolated or extrapolated from local rarefaction curves

m <- c(1,2,3,4,5,6,9,11,13,15,18,27,36)

m
# Interpolation and extrapolation using iNEXT

inext_data2 <- iNEXT(t_lizards_abund, q = 0, datatype = "abundance", size = m)
inext_data2

# Extract iNEXT estimates

extrapolated_data2 <- inext_data2$iNextEst
extrapolated_data2
ext_dat2 <- do.call("rbind",extrapolated_data2)

#lizards_abund <- read_csv("lizards_andamans - abund.csv")

div_local$a_Sn <- ext_dat2$qD

alpha_lizards <- div_local %>%
  group_by(Site) %>%
  summarise_all(mean)


write.csv2(alpha_lizards, "/Users/leanagooriah/Downloads/local_lizards.csv")
