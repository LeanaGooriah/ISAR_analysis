# Interpolating and extrapolating alpha & gamma Sn values
# Example using case study 3 - fragmentation data 

#------------------------------------------------------------------------------------------
##### ALPHA SN ESTIMATES #####
# load transect level data and iNEXT

library(iNEXT)
frag_abund <- read_csv("Desktop/ISAR_DATA/datasets/forest_fragments_data.csv")

# Transpose data to fit iNEXT data format requirement
Site_names <- frag_abund$Site
frag_abund$Site <- NULL

t_frag_abund<- t(frag_abund)
t_frag_abund <- as.data.frame(t_frag_abund)


# Get average N values where Sn will be interpolated or extrapolated 
#from local rarefaction curves (one curve per transect per fragment)
n <- round(mean(rowSums(frag_abund)))

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
write.csv2(average_Sn_alpha, "/Users/leanagooriah/desktop/ISAR_DATA/alpha_Sn_frag.csv")


###### GAMMA SN ESTIMATES #####
frag_abund <- read_csv("Desktop/ISAR_DATA/datasets/forest_fragments_data.csv")

# Calculate summed up abundance 
glade_sum <- frag_abund %>%
  group_by(Site) %>%
  summarise_all(sum)

# transpose data
Site_names <- glade_sum$Site
glade_sum$Site <- NULL
t_glade_sum <- t(glade_sum)
t_glade_sum <- as.data.frame(t_glade_sum) 


##### Interpolation and extrapolation using iNEXT #####

inext_data_g <- iNEXT(t_glade_sum, q = 0, datatype = "abundance", size = m)
inext_data_g 

extrapolated_data_g <- inext_data_g$iNextEst
extrapolated_data_g
ext_dat_g <- do.call("rbind",extrapolated_data_g)
ext_dat_g <- subset(ext_dat_g, m == n)
ext_dat_g$method <- NULL
ext_dat_g$Site <- Site_names
# Save iNEXT gamma Sn estimates 
write.csv2(ext_dat_g, "/Users/leanagooriah/desktop/ISAR_DATA/gamma_frag.csv")


