
# Interpolating and extrapolating alpha Sn values

# load transect level data

lizards_abund <- read_csv("lizards_andamans - abund.csv")


# Transpose data to fit iNEXT data format requirement
t_lizards_abund<- t(lizards_abund)
t_lizards_abund <- as.data.frame(t_lizards_abund)


# Get average N values where Sn will be interpolated or extrapolated from local rarefaction curves

m <- c(1,2,3,4,5,6,9,11,13,15,18,27,36)

##### Interpolation and extrapolation using iNEXT #####

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