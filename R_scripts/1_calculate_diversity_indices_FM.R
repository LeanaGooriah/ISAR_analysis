#####################################################################################
# Author : Leana Gooriah
# Email : leana_devi.gooriah@idiv.de
#####################################################################################
# Description : This script can be used to calculate the following diversity indices:
# Stotal, Sn, S_PIE, Beta_Sn, Beta_S_PIE (following 'mobr' terminology)
# at two different scales: total (gamma) and local (alpha)
#####################################################################################
# 1. load data
# 2. calculate S_PIE and BETA_PIE at local scales and S_total, S_PIE, and BETA_PIE at
#    at total scales
# 3. calculate n for local and total scales
# 4. calculate Sn and Beta_Sn for local and total scales

# LOAD R PACKAGES

require(dplyr)
require(tidyr)
#library(devtools)
#install_github('MoBiodiv/mobr') # to install most recent version of mobr
require(mobr) # version 1.0

############################
# Set path and directories #
############################

work_dir <- getwd() # first set working directory in Menu/Session/Set working directory/to Project 
data_path <- paste(work_dir,"/ISAR_DATA/datasets",sep="")

# Read all data file names
filenames <- list.files(data_path, pattern="*.csv*", full.names = F)

# Make list of study ids
filename_roots <- gsub(".csv","",filenames) # remove .csv from filenames

study_ids <- unique(filename_roots)

###################
# 1.Read in data  #
###################

n_files <- length(unique(study_ids))
data_out <- list()

for (i in 1:n_files){
  data_file <- paste(data_path,"/", study_ids[i], ".csv", sep ="")
  data_out[[i]] <- read.csv(data_file, header = TRUE, stringsAsFactors = F)
}

##################################################################
# 2. Calculate Alpha and Gamma (S_n, S_asymp,S_PIE, BETA_PIE)    #
##################################################################
# "S_asymp" is "S_total" (in the manuscript) and            #
# is estimated as asymptotic species richness at the        #
# at the gamma level                                        #  
#############################################################

div_out <- list()

for (i in 1:n_files){

  alpha_tab <- data_out[[i]]
  alpha_tab$Site <- as.character(alpha_tab$Site)
  
  # count samples per site
  samples_in_sites <- alpha_tab %>%
    group_by(Site) %>%
    count()
  
  # remove island/fragments with just one sample
  alpha_tab2 <- alpha_tab %>%
    left_join(samples_in_sites) %>%
    filter(n > 1) %>%
    select(-n)
  
  rm(alpha_tab)
 
  # site by species table at the gamma scale
  gamma_tab <- alpha_tab2 %>% 
    group_by(Site) %>%
    summarise_all(sum) %>%
    ungroup()
  class(gamma_tab) <- ("data.frame")
  
  # estimate reference n for rarefaction and extrapolations
  n_sites <- rowSums(alpha_tab2[,-1])
  r <- 2
  max_n <- max(n_sites)
  min_n_r <- min(r * n_sites)
  n_ref <- max(max_n, min_n_r)
  
  alpha_div <- calc_biodiv(alpha_tab2[,-1],
                           groups = alpha_tab2$Site,
                           index = c("S_n","S_PIE"),
                           effort = n_ref,
                           extrapolate = T,
                           return_NA = F)
  
  gamma_div <- calc_biodiv(gamma_tab[,-1], 
                           groups = gamma_tab$Site,
                           index = c("S_n","S_asymp","S_PIE"),
                           effort = n_ref,
                           extrapolate = T,
                           return_NA = F)
  
  # calculate beta diversity
  beta_div <- alpha_div %>%
    left_join(gamma_div, by = c("group", "index")) %>%
    mutate(value = value.y/value.x,
           index = paste("beta_",index, sep = "")) %>%
    select(group, index, value)
             
  alpha_div_mean <- alpha_div %>% 
    group_by(group, index) %>%
    summarise(value = mean(value, na.rm = T))
  
  beta_div_mean <- beta_div %>% 
    group_by(group, index) %>%
    summarise(value = mean(value, na.rm = T))
  
  # Add Study IDs  
  alpha_div_mean$Study <- study_ids[i]
  beta_div_mean$Study <- study_ids[i]
  gamma_div$Study <- study_ids[i]
  
  # Add scales
  alpha_div_mean$Scale <- "alpha"
  beta_div_mean$Scale <- "beta"
  gamma_div$Scale <- "gamma"
  
  div_out[[i]] <- bind_rows(alpha_div_mean, beta_div_mean, gamma_div)
}

div_out <- bind_rows(div_out)
div_out <- select(div_out, Study, Scale, Site = group, index, value)
div_out$index <- ifelse(div_out$index == "S_asymp", "S_total", div_out$index)


##########################################
# Clean, join files and save output      #
# File 1: all studies, diversity indices #
# File 2: sampling effort for Sn for both#
#          scales                        #
##########################################

div_out <- filter(div_out, is.na(value) == FALSE) # remove NaN (where PIE could not be calculated)

div_out$Scale = factor(div_out$Scale, levels = c("gamma","alpha","beta"))
div_out$index = factor(div_out$index, levels=c("S_total","S_n","S_PIE","beta_S_n","beta_S_PIE"))

div_out <- arrange(div_out, Study, Scale, index)

#############################
# write out clean data file #
#############################
write.csv(div_out, "ISAR_DATA/diversity_indices/allstudies_allscales_allindices.csv", row.names=FALSE)

# File 2: sampling effort calculation of effort for Sn at gamma and alpha scales
# # 
# 
# alpha_effort<-summarise(group_by(alpha_n_out, Study), effort=unique(effort))
# alpha_effort$Scale<-"alpha"
# 
# gamma_effort<-summarise(group_by(gamma_n_out, Study), effort=unique(effort))
# gamma_effort$Scale<-"gamma"
# 
# effort<-rbind.data.frame(alpha_effort,gamma_effort)
# effort<-arrange(effort, Study, Scale)
# 
# write.csv(effort, "ISAR_DATA/diversity_indices/allstudies_Sn_samplingeffort.csv",row.names=FALSE)
