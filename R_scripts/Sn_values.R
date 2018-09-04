
# Interpolating and extrapolating alpha Sn values

# Get average N values from which Sn will be interpolated or extrapolated from local rarefaction curves
# load diversity file

m <- round(diversity_indices$N)

##### Interpolation and extrapolation using iNEXT #####

# load transect level data

lizards_abund <- read_csv("lizards_andamans - abund.csv")


# Transpose data to fit iNEXT data format requirement
t_lizards_abund<- t(lizards_abund)
t_lizards_abund <- as.data.frame(t_lizards_abund)

inext_data2 <- iNEXT(t_lizards_abund, q = 0, datatype = "abundance", size = m)
inext_data2

# Extract iNEXT estimates per transect per Site/Island

extrapolated_data2 <- inext_data2$iNextEst
extrapolated_data2
ext_dat2 <- do.call("rbind",extrapolated_data2)

#lizards_abund <- read_csv("lizards_andamans - abund.csv")
div_local$a_Sn <- ext_dat2$qD

# Get average values of Sn per Site

alpha_lizards <- div_local %>%
  group_by(Site) %>%
  summarise_all(mean)


write.csv2(alpha_lizards, "/Users/leanagooriah/Downloads/local_lizards.csv")
