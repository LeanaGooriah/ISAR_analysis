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

alpha_out <- list()
beta_out <- list()
gamma_out <- list()

for (i in 1:n_files){

  #  convert data to mobr format
  alpha_tab <- data_out[[i]]
  alpha_tab$Site <- as.character(alpha_tab$Site)
 
  # site by species table at the gamma scale
  gamma_tab <- alpha_tab %>% 
    group_by(Site) %>%
    summarise_all(sum) %>%
    ungroup()
  class(gamma_tab) <- ("data.frame")
  
  # estimate reference n for rarefaction and extrapolations
  n_sites <- rowSums(alpha_tab[,-1])
  r <- 2
  max_n <- max(n_sites)
  min_n_r <- min(r * n_sites)
  n_ref <- max(max_n, min_n_r)
  
  alpha_div <- calc_biodiv(alpha_tab[,-1],
                           groups = alpha_tab$Site,
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
  
  gamma_out[[i]] <- gamma_div
  alpha_out[[i]] <- alpha_div_mean
  beta_out[[i]] <- beta_div_mean
}

gamma_out <- bind_rows(gamma_out)
gamma_out <- select(gamma_out, Study, Site = group, index, value)

alpha_out <- bind_rows(alpha_out)
alpha_out <- select(alpha_out, Study, Site = group, index, value)

beta_out <- bind_rows(beta_out)
beta_out <- select(beta_out, Study, Site = group, index, value)



##########################################
# Clean, join files and save output      #
# File 1: all studies, diversity indices #
# File 2: sampling effort for Sn for both#
#          scales                        #
##########################################

# File 1: save output for analysis and figures

gamma_pie_out$Scale<-"gamma" 
gamma_pie_out$index<-as.character(gamma_pie_out$index) # change "S_asymp" to "S_total"
gamma_pie_out$index<-ifelse(gamma_pie_out$index=="S_asymp","S_total",gamma_pie_out$index)
alpha_pie_out$Scale<-"alpha"

pie_out<-rbind.data.frame(gamma_pie_out, alpha_pie_out)
pie_out$Scale<-as.character(pie_out$Scale)
pie_out$index<-as.character(pie_out$index)

pie_out$Scale<-ifelse(pie_out$index=="beta_S_PIE","beta",pie_out$Scale)

gamma_n_outt<-select(gamma_n_out, Study, Site=group, value=gamma_Sn)
gamma_n_outt$Scale<-"gamma"
gamma_n_outt$index<-"Sn"

alpha_n_outt<-select(alpha_n_out, Study, Site=group, value=Sn)
alpha_n_outt$Scale<-"alpha"
alpha_n_outt$index<-"Sn"

n_out<-rbind.data.frame(gamma_n_outt,alpha_n_outt)

beta_Sn<-select(beta_Sn, Study, Site=group, value=beta_Sn)
beta_Sn$index<-"beta_Sn"
beta_Sn$Scale<-"beta"

diversity_out<-rbind.data.frame(pie_out, n_out)
diversity_out<-rbind.data.frame(diversity_out, beta_Sn)

diversity_out<-filter(diversity_out, is.na(value)==FALSE) # remove NaN (where PIE could not be calculated)

diversity_out$Scale = factor(diversity_out$Scale, levels=c("gamma","alpha","beta"))
diversity_out$index = factor(diversity_out$index, levels=c("S_total","Sn","S_PIE","beta_Sn","beta_S_PIE"))

diversity_out<-arrange(diversity_out, Study, Scale, index)

#######################################
# Remove beta diversity values from   #
# islands/fragments with only 1 plot  #
#######################################

study_a<-data_out[[1]]
study_a<-summarise(group_by(study_a, Site), PlotN=length(Site))
study_a<-filter(study_a, PlotN<2) # all islands/fragments > 1 plot

study_b<-data_out[[2]]
study_b<-summarise(group_by(study_b, Site), PlotN=length(Site))
study_b$Site<-as.character(study_b$Site)
study_b<-filter(study_b, PlotN<2)
study_b$Study<-"lizards_islands_SurendranVasudevan"

study_c<-data_out[[3]]
study_c<-summarise(group_by(study_c, Site), PlotN=length(Site))
study_c$Site<-as.character(study_c$Site)
study_c<-filter(study_c, PlotN<2)
study_c$Study<-"plants_habitatfragments_Giladi"

drop_beta<-rbind.data.frame(study_b,study_c)
drop_beta$Scale<-"beta"

drop_beta<-drop_beta%>%
           unite("comb", Study, Site, Scale,sep=":",remove=FALSE)

rem_beta<-as.character(drop_beta$comb) # vector to ID

diversity_out<-diversity_out%>%
  unite("comb", Study, Site, Scale,sep=":",remove=FALSE)

diversity_out<-filter(diversity_out, !comb %in% rem_beta ) # remove beta Sn and beta S_PIE
diversity_out$comb<-NULL

#############################
# write out clean data file #
#############################
write.csv(diversity_out, "ISAR_DATA/diversity_indices/allstudies_allscales_allindices.csv",row.names=FALSE)

# File 2: sampling effort calculation of effort for Sn at gamma and alpha scales

alpha_effort<-summarise(group_by(alpha_n_out, Study), effort=unique(effort))
alpha_effort$Scale<-"alpha"

gamma_effort<-summarise(group_by(gamma_n_out, Study), effort=unique(effort))
gamma_effort$Scale<-"gamma"

effort<-rbind.data.frame(alpha_effort,gamma_effort)
effort<-arrange(effort, Study, Scale)

write.csv(effort, "ISAR_DATA/diversity_indices/allstudies_Sn_samplingeffort.csv",row.names=FALSE)
