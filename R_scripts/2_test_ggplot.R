#######################
# make figures ########
# trial ###############
#######################

require(ggplot2)
require(dplyr)

###################
# 1.Read in data  #
###################

#  add island/fragment area to diversity indices

area_study<-read.csv("ISAR_DATA/island_area_data.csv",header=TRUE)
area_study<-select(area_study, Study=case_study_ID,Site=island_name, Area,unit=unit..area.)
area_study$Area<-ifelse(area_study$unit=="ha",area_study$Area/100,area_study$Area) # convert ha to km2
area_study<-select(area_study, -unit)

area_study$Study<-as.character(area_study$Study)
area_study$Study<-ifelse(area_study$Study=="andamans_data","lizards_islands_SurendranVasudevan",area_study$Study)
area_study$Study<-ifelse(area_study$Study=="glades_data","grasshopper_glades_RybergChase",area_study$Study)
area_study$Study<-ifelse(area_study$Study=="forest_fragments_data","plants_habitatfragments_Giladi",area_study$Study)

div_indices<-read.csv("ISAR_DATA/diversity_indices/allstudies_allscales_allindices.csv",header=T)
div_indices_area<-left_join(div_indices, area_study, by.y=c("Study","Site"))

write.csv(div_indices_area,"ISAR_DATA/diversity_indices/allstudies_allscales_allindices_witharea.csv",row.names=FALSE) # save for linear models

################
# make figures #
################

div_indices_area$Study<-as.factor(div_indices_area$Study)
div_indices_area$Study = factor(div_indices_area$Study, levels=c("lizards_islands_SurendranVasudevan","grasshopper_glades_RybergChase",
                                                                 "plants_habitatfragments_Giladi"))
################
# Gamma scale  #
################
gamma<-filter(div_indices_area, Scale=="gamma")
gamma$index = factor(gamma$index, levels=c("S_total","Sn","S_PIE"))

gamma_a<-filter(gamma, Study=="lizards_islands_SurendranVasudevan")
gamma_b<-filter(gamma, Study=="grasshopper_glades_RybergChase")
gamma_c<-filter(gamma, Study=="plants_habitatfragments_Giladi")

# Lizards
gamma_A_f<-ggplot(data = gamma_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                           labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                                                         "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"Sn"=17, "S_PIE" = 16),
                    labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+ scale_y_continuous(trans = "log10") +
 
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text=element_text(size=12),
                   axis.title.x=element_text(color="transparent"),
                   axis.title.y=element_text(size=14,face="bold"),
                   legend.title=element_blank(), 
                  legend.position=c(0.18,0.82),
                  plot.margin = unit(c(0.5,0,0,0), "cm"))

# Grasshoppers
gamma_B_f<-ggplot(data = gamma_b, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                    labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"Sn"=17, "S_PIE" = 16),
                     labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                              "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+ scale_y_continuous(trans = "log10") +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text=element_text(size=12),
                   axis.title.x=element_text(color="transparent"),
                   axis.title.y=element_text(size=14,face="bold"),
                   legend.title=element_blank(), 
                   legend.position="none")

# Plants
gamma_C_f<-ggplot(data = gamma_c, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","Sn"="#5ab4ac", "S_PIE" = "#d8b365"),
                    labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"Sn"=17, "S_PIE" = 16),
                     labels=c("S_total"=expression(S["total"]),"Sn"=expression(paste(gamma,"S"["n"])),
                              "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",limits=c(0.0017,0.04),breaks = c(0.002, 0.004, 0.01, 0.03 ))+ scale_y_continuous(trans = "log10") +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=14,face="bold"),
                   legend.title=element_blank(), 
                   legend.position="none")

########### 
# Alpha   #
###########

alpha<-filter(div_indices_area, Scale=="alpha")
alpha$index = factor(alpha$index, levels=c("Sn","S_PIE"))

alpha_a<-filter(alpha, Study=="lizards_islands_SurendranVasudevan")
alpha_b<-filter(alpha, Study=="grasshopper_glades_RybergChase")
alpha_c<-filter(alpha, Study=="plants_habitatfragments_Giladi")

# Lizards
alpha_A_f<-ggplot(data = alpha_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("Sn"=17, "S_PIE" = 16),
                     labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                              "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+ scale_y_continuous(trans = "log10",limits=c(0.9,14)) +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_text(size=12,colour="transparent"),
                   axis.title.x=element_text(color="transparent"),
                   axis.title.y=element_text(size=14,colour="transparent"),
                   legend.title=element_blank(), 
                   legend.position=c(0.18,0.82),
                   plot.margin = unit(c(0.5,0,0,0), "cm"))

# Grasshoppers
alpha_B_f<-ggplot(data = alpha_b, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("Sn"=17, "S_PIE" = 16),
                     labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                              "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  
  
  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+  scale_y_continuous(trans = "log10",limits=c(1,30)) +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_blank(),
                   axis.title.x=element_blank(), 
                   axis.title.y=element_blank(), 
                   legend.title=element_blank(), 
                   legend.position="none")

# Plants
alpha_C_f<-ggplot(data = alpha_c, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("Sn"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("Sn"=17, "S_PIE" = 16),
                     labels=c("Sn"=expression(paste(alpha,"S"["n"])),
                              "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",limits=c(0.0017,0.04),breaks = c(0.002, 0.004, 0.01, 0.03 ))+ 
          scale_y_continuous(trans = "log10", limits=c(1,47)) +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_blank(),
                   axis.title.x=element_text(size=14,face="bold"),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(), 
                   legend.position="none")

#########
# Beta ##
#########

beta<-filter(div_indices_area, Scale=="beta")
beta$index = factor(beta$index, levels=c("beta_Sn","beta_S_PIE"))

beta_a<-filter(beta, Study=="lizards_islands_SurendranVasudevan")
beta_b<-filter(beta, Study=="grasshopper_glades_RybergChase")
beta_c<-filter(beta, Study=="plants_habitatfragments_Giladi")

# Lizards
beta_A_f<-ggplot(data = beta_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_Sn"=17, "beta_S_PIE" = 16),
                     labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                              "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+

  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+ scale_y_continuous(trans = "log10",limits=c(0.85,14)) +
  

  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(), 
                   legend.position=c(0.18,0.82),
                   plot.margin = unit(c(0.5,0,0,0), "cm"))

# Grasshoppers
beta_B_f<-ggplot(data = beta_b, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_Sn"=17, "beta_S_PIE" = 16),
                     labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                              "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  
  
  scale_x_continuous(trans = "log10",breaks = c(1, 100, 1000,10000))+  scale_y_continuous(trans = "log10",limits=c(0.95,30)) +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text.x=element_text(size=12),
                   axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(), 
                   legend.position="none")

# Plants
beta_C_f<-ggplot(data = beta_c, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_Sn"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_Sn"=17, "beta_S_PIE" = 16),
                     labels=c("beta_Sn"=expression(paste(beta,"S"["n"])),
                              "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",limits=c(0.0017,0.04),breaks = c(0.002, 0.004, 0.01, 0.03 ))+ 
  scale_y_continuous(trans = "log10", limits=c(0.95,47)) +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text=element_text(size=12),
                   axis.text.y=element_blank(),
                   axis.title.x=element_text(size=14,face="bold"),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(), 
                   legend.position="none")

###############
# combine #####
###############

require(cowplot)

isar_tog<-plot_grid(gamma_A_f,alpha_A_f,beta_A_f,
                        gamma_B_f,alpha_B_f,beta_B_f,
                        gamma_C_f,alpha_C_f,beta_C_f,
                        labels=c("A) Total","B) Local","C) Beta",
                                 "D) Total","E) Local","F) Beta",
                                 "G) Total","H) Local","I) Beta"),
                         label_size=10, label_fontface = "bold", 
                      hjust=-1.3, vjust=1, ncol=3, align="hv")

ggsave(filename = "FIGURE/isar_figure_DCtest.png", plot = isar_tog, 
        device = "png",dpi=500, width = 25, height = 25, units = "cm")


