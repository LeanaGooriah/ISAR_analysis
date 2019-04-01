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
filename_roots <- gsub(".csv","",filenames) # remove _data from filenames

study_ids <- unique(filename_roots)

###################
# 1.Read in data  #
###################

s<-length(unique(study_ids))
data_out<-list()

for (i in 1:s){
  
  data_file <- paste(data_path,"/", study_ids[i], ".csv", sep ="")
  data_out[[i]] <- read.csv(data_file, header = TRUE)
}

#############################################################
# 2. Calculate Alpha and Gamma (S_asymp,S_PIE, BETA_PIE)    #
#############################################################
# "S_asymp" is "S_total" (in the manuscript) and            #
# is estimated as asymptotic species richness at the        #
# at the gamma level                                        #  
#############################################################

gamma_out<-list(); alpha_out<-list()

for (i in 1:s){

#  convert data to mobr format

datt<-data_out[[i]]
datt[,1]<-as.character(datt[,1])
datt_m<-datt[,-1] # sites by species table

group<-datt[,1] # group table
group<-data.frame(group)
group$PlotID<-rownames(group)
rownames(group)<-group$PlotID
group<-select(group,-PlotID)
group$group<-as.factor(group$group)

group$x<-NA  
group$x<-as.numeric(group$x)

group$y<-NA  
group$y<-as.numeric(group$y)

# calculate diversity indices

h_mob_in <- make_mob_in(datt_m, group)

h_stats <- get_mob_stats(h_mob_in, group_var = "group", 
                         index = c("S_asymp","S_PIE"),
                         boot_groups=TRUE, conf_level=0.95,nperm=999)

# gamma level Stotal and S_PIE
gamma_pie_total<-h_stats$groups_stats
gamma_pie_total<-ungroup(gamma_pie_total)
gamma_pie_total<-select(gamma_pie_total,group, index, lower,median, upper)
gamma_pie_total$Study<-study_ids[i]

# alpha level S_PIE
local_pie<-h_stats$samples_stats

local_pie<-filter(local_pie, index=="S_PIE"|index=="beta_S_PIE")%>%
           group_by(group, index)%>%
           summarise(value=mean(value, na.rm=TRUE))

local_pie$Study<-study_ids[i]

gamma_out[[i]]<-gamma_pie_total
alpha_out[[i]]<-local_pie
}

gamma_pie_out<-do.call(rbind.data.frame,gamma_out)
gamma_pie_out<-select(gamma_pie_out, Study, Site=group, index, value=median)
alpha_pie_out<-do.call(rbind.data.frame,alpha_out)
alpha_pie_out<-select(alpha_pie_out, Study, Site=group, index, value)
alpha_pie_out$index<-droplevels(alpha_pie_out$index)

################################################
# 3. Estimate Sn at gamma and alpha levels     #
#     following Chao et al. 2014               #
################################################

######################################################
# alpha scale                                        #  
# a. calculated rarefied species richness per plot   #
#    where n is defined by Chao et al. 2014          #   
# b. calculate mean Sn for each island/fragment      #
######################################################

alpha_n_out<-list()

for (j in 1:s){
  
  #  convert data to mobr format
  
  dat_a<-data_out[[j]]
  rows<-nrow(dat_a)
  dat_a[,1]<-as.character(dat_a[,1])
  dat_mm<-dat_a[,-1] # sites by species table

  # estimate minimum n per site
  n_sites <- rowSums(dat_mm)
  r <- 2
  max_n <- max(n_sites)
  min_n_r <- min(r * n_sites)
  n_local <- max(max_n, min_n_r)
  plot_Snnn<-list()
for (k in 1:rows){
  plot<-dat_mm %>% slice(k)
  plot_Sn<-rarefaction(plot, method='indiv', effort=n_local, extrapolate = TRUE, quiet_mode=TRUE)
  plot_Snn<-cbind(k, plot_Sn,effort=n_local)
  plot_Snnn[[k]]<-rbind.data.frame(plot_Snn)
  
}
  plot_Snnn<-do.call(rbind.data.frame,plot_Snnn)
  plot_Snnn$Study<-study_ids[j]
  plot_Snnn$group<-dat_a[,1]
  alpha_n_out[[j]]<-rbind.data.frame(plot_Snnn)
}  

alpha_n_out<-do.call(rbind.data.frame,alpha_n_out)

alpha_n_out<-alpha_n_out %>%
              group_by(Study, group)%>%
              summarise(effort=unique(effort), Sn=mean(plot_Sn))

##################################################################
# gamma scale                                                    #  
#    calculated rarefied species richness per  island/fragment   #
#    island/fragmentwhere n is defined by Chao et al. 2014       #
##################################################################

gamma_n_out<-list()

for (l in 1:s){
  
  #  convert data to mobr format
  
  dat_b<-data_out[[l]]
  
  # Sum abundance per island/fragment
  
  dat_bb <- dat_b %>%
    group_by(Site) %>%
    summarise_all(sum)
  
  rowss<-nrow(dat_bb) # number of islands/fragments
  dat_bb<-ungroup(dat_bb)
  dat_mmm<-dat_bb[,-1] # sites by species table
  
  # estimate minimum n per site
  n_sitess <- rowSums(dat_mmm)
  r <- 2
  max_n <- max(n_sitess)
  min_n_r <- min(r * n_sitess)
  n_gamma <- max(max_n, min_n_r)

  gamma_Snnn<-list()
  
  for (m in 1:rowss){
    island<-dat_mmm %>% slice(m)
    island<-ungroup(island)
    gamma_Sn<-rarefaction(island, method='indiv', effort=n_gamma, extrapolate = TRUE, quiet_mode=TRUE)
    gamma_Snn<-cbind(m, gamma_Sn,effort=n_gamma)
    gamma_Snnn[[m]]<-rbind.data.frame(gamma_Snn)
    
  }
  gamma_Snnn<-do.call(rbind.data.frame,gamma_Snnn)
  gamma_Snnn<-ungroup(gamma_Snnn)
  gamma_Snnn$Study<-study_ids[l]
  gamma_Snnn$group<-as.character(unique(dat_bb$Site))
  gamma_n_out[[l]]<-rbind.data.frame(gamma_Snnn)
}  

gamma_n_out<-do.call(rbind.data.frame,gamma_n_out)

##################################################################
# beta Sn:  gamma Sn/ alpha Sn                                   #  
#    calculated rarefied species richness per  island/fragment   #
#    island/fragmentwhere n is defined by Chao et al. 2014       #
##################################################################

beta_Sn_a<-select(alpha_n_out, Study, group, alpha_Sn=Sn)
beta_Sn_g<-select(gamma_n_out, Study, group, gamma_Sn=gamma_Sn)

beta_Sn<-left_join(beta_Sn_a, beta_Sn_g, by.y=c("Study","group"))
beta_Sn$beta_Sn<-beta_Sn$gamma_Sn/beta_Sn$alpha_Sn
beta_Sn<-select(beta_Sn, Study, group, beta_Sn)  

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