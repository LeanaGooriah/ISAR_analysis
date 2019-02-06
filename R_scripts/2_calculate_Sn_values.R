# Interpolating and extrapolating alpha & gamma Sn values
# Example using case study 3 - fragmentation data 

#------------------------------------------------------------------------------------------
# ALPHA SN ESTIMATES
# load transect level data and iNEXT

library(iNEXT)
frag_abund <- read_csv("~/ISAR_DATA/fragments.csv")

# Transpose data to fit iNEXT data format requirement
t_frag_abund<- t(frag_abund)
t_frag_abund <- as.data.frame(t_frag_abund)
colnames(t_frag_abund) <- as.character(t_frag_abund[1,])


# Get average N values where Sn will be interpolated or extrapolated 
#from local rarefaction curves (one curve per transect per fragment)
m <- (mean(rowSums(frag_abund)))

##### Interpolation and extrapolation using iNEXT #####

inext_data_a <- iNEXT(t_frag_abund, q = 0, datatype = "abundance", size = m)
inext_data_a

# Extract iNEXT estimates (Sn values = qd column
# Sn values (qd column in exdat2) at specific N (number of individuals) referred to as "m" 

extrapolated_data_a <- inext_data_a$iNextEst
extrapolated_data_a
ext_dat_a <- do.call("rbind",extrapolated_data_a)

# Save iNEXT Sn estimates and calculate average Sn values per fragment
write.csv2(ext_dat_a, "/Users/leanagooriah/ISAR_DATA/alpha_frag.csv")


# GAMMA SN ESTIMATES

# Calculate summed up abundance 
frag_sum <- frag_abund %>%
  group_by(Site) %>%
  summarise_all(sum)

# transpose data

t_frag_sum <- t(frag_sum)
t_frag_sum <- as.data.frame(t_frag_sum) 
colnames(t_frag_sum) <- as.character(t_frag_sum[1,])


##### Interpolation and extrapolation using iNEXT #####

inext_data_g <- iNEXT(t_frag_abund, q = 0, datatype = "abundance", size = m)
inext_data_g 

extrapolated_data_g <- inext_data_g$iNextEst
extrapolated_data_g
ext_dat_g <- do.call("rbind",extrapolated_data_g)

# Save iNEXT gamma Sn estimates 
write.csv2(ext_dat_g, "/Users/leanagooriah/ISAR_DATA/gamma_frag.csv")




