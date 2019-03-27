# Interpolating and extrapolating alpha & gamma Sn values
# Example using case study 3 - fragmentation data 

#------------------------------------------------------------------------------------------
##### ALPHA SN ESTIMATES #####
# load transect level data and iNEXT

library(iNEXT)
library(tidyverse)

# please set all path relative to the GitHub repo folder 
# to do that please do the following in RStudio:
# Menu Session --> Set working directory --> To project directory
work_dir <- getwd()

frag_abund <- read_csv("ISAR_DATA/datasets/forest_fragments_data.csv")

# The file is called forest fragments, but the species names are really familier to me.
# Apparently this is data from my project in Israel. Is this correct???

# Transpose data to fit iNEXT data format requirement
Site_names <- frag_abund$Site
frag_abund$Site <- NULL

t_frag_abund<- t(frag_abund)
t_frag_abund <- as.data.frame(t_frag_abund)

# Implement approach by Chao et al. 2014
n_sites <- rowSums(frag_abund)

r <- 2

max_n <- max(n_sites)
min_n_r <- min(r * n_sites)
n <- max(max_n, min_n_r)
# --------------------

# Get average N values where Sn will be interpolated or extrapolated 
#from local rarefaction curves (one curve per transect per fragment)
#n <- round(mean(rowSums(frag_abund)))

m <- c(10,20,n)

##### Interpolation and extrapolation using iNEXT #####

inext_data_a <- iNEXT(t_frag_abund, q = 0, datatype = "abundance", size = m)
inext_data_a

# Extract iNEXT estimates (Sn values = qd column
# Sn values (qd column in exdat2) at specific N (number of individuals) referred to as "m" 

extrapolated_data_a <- inext_data_a$iNextEst
extrapolated_data_a
ext_dat_a <- do.call("rbind",extrapolated_data_a)

ext_dat_a <- subset(ext_dat_a, m == n)
ext_dat_a$method <- NULL
ext_dat_a$Site <- Site_names

average_Sn_alpha <- ext_dat_a %>%
  group_by(Site) %>%
  summarise_all(mean)

# Save iNEXT Sn estimates and calculate average Sn values per fragment
outfile <- paste(work_dir, "/ISAR_DATA/alpha_Sn_frag.csv", sep = "") 
write.csv2(average_Sn_alpha, outfile)


###### GAMMA SN ESTIMATES #####

frag_abund <- read_csv("ISAR_DATA/datasets/forest_fragments_data.csv")

# Calculate summed up abundance 
frag_sum <- frag_abund %>%
  group_by(Site) %>%
  summarise_all(sum)

# transpose data
Site_names <- frag_sum$Site
frag_sum$Site <- NULL
t_frag_sum <- t(frag_sum)
t_frag_sum <- as.data.frame(t_frag_sum) 


##### Interpolation and extrapolation using iNEXT #####

inext_data_g <- iNEXT(t_frag_sum, q = 0, datatype = "abundance", size = m)
inext_data_g 

extrapolated_data_g <- inext_data_g$iNextEst
extrapolated_data_g
ext_dat_g <- do.call("rbind",extrapolated_data_g)
ext_dat_g <- subset(ext_dat_g, m == n)
ext_dat_g$method <- NULL
ext_dat_g$Site <- Site_names
# Save iNEXT gamma Sn estimates 

outfile <- paste(work_dir, "/ISAR_DATA/gamma_frag.csv", sep = "") 
write.csv2(ext_dat_g, outfile)


