require(ggplot2)
require(dplyr)

################
# make figures #
################
allstudies_allscales_allindices <- read_csv("diversity_indices/allstudies_allscales_allindices.csv")
allstudies_allscales_allindices$Study<-as.factor(allstudies_allscales_allindices$Study)
allstudies_allscales_allindices$Study = factor(allstudies_allscales_allindices$Study, levels=c("lizards_islands_SurendranVasudevan","grasshopper_glades_RybergChase",
                                                                 "plants_habitat_fragments_Giladi_et_al"))
################
# Gamma scale  #
################
gamma<-filter(allstudies_allscales_allindices, Scale=="gamma")
gamma$index = factor(gamma$index, levels=c("S_total","S_n","S_PIE"))

gamma_a<-filter(gamma, Study=="lizards_islands_SurendranVasudevan")
gamma_b<-filter(gamma, Study=="grasshopper_glades_RybergChase")
gamma_c<-filter(gamma, Study=="plants_habitat_fragments_Giladi_et_al")
gamma_c$Area <- gamma_c$Area/100

# Lizards
gamma_A_f<-ggplot(data = gamma_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                    labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"S_n"=17, "S_PIE" = 16),
                     labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                              "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+ scale_y_continuous(trans = "log10") +
  
  labs(x=expression(bold(paste("Area (km" ^2,")"))), y="Species Number") +
  theme_bw()+theme(aspect.ratio = 1,
                   axis.text=element_text(size=12),
                   axis.title.x=element_text(color="transparent"),
                   axis.title.y=element_text(size=14,face="bold"),
                   legend.title=element_blank(), 
                   legend.position=c(0.18,0.80),
                   plot.margin = unit(c(0.5,0,0,0), "cm"))

# Grasshoppers
gamma_B_f<-ggplot(data = gamma_b, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                    labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"S_n"=17, "S_PIE" = 16),
                     labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                              "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+ scale_y_continuous(trans = "log10") +
  
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
  
  scale_colour_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                      labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                               "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  
  scale_fill_manual(name="",values = c("S_total" = "#d73027","S_n"="#5ab4ac", "S_PIE" = "#d8b365"),
                    labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
                             "S_PIE"=expression(paste(gamma,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_total" = 8,"S_n"=17, "S_PIE" = 16),
                     labels=c("S_total"=expression(S["total"]),"S_n"=expression(paste(gamma,"S"["n"])),
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

alpha<-filter(allstudies_allscales_allindices, Scale=="alpha")
alpha$index = factor(alpha$index, levels=c("S_n","S_PIE"))

alpha_a<-filter(alpha, Study=="lizards_islands_SurendranVasudevan")
alpha_b<-filter(alpha, Study=="grasshopper_glades_RybergChase")
alpha_c<-filter(alpha, Study=="plants_habitat_fragments_Giladi_et_al")
alpha_c$Area <- alpha_c$Area/100

# Lizards
alpha_A_f<-ggplot(data = alpha_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_n"=17, "S_PIE" = 16),
                     labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                              "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+
  scale_y_continuous(trans = "log10",limits=c(0.7,14)) +
  
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
  
  scale_colour_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_n"=17, "S_PIE" = 16),
                     labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                              "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+  scale_y_continuous(trans = "log10",limits=c(1,30)) +
  
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
  
  scale_colour_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                      labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                               "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("S_n"="#7fbf7b", "S_PIE" = "#fc8d59"),
                    labels=c("S_n"=expression(paste(alpha,"S"["n"])),
                             "S_PIE"=expression(paste(alpha,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("S_n"=17, "S_PIE" = 16),
                     labels=c("S_n"=expression(paste(alpha,"S"["n"])),
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

beta<-filter(allstudies_allscales_allindices, Scale=="beta")
beta$index = factor(beta$index, levels=c("beta_S_n","beta_S_PIE"))

beta_a<-filter(beta, Study=="lizards_islands_SurendranVasudevan")
beta_b<-filter(beta, Study=="grasshopper_glades_RybergChase")
beta_c<-filter(beta, Study=="plants_habitat_fragments_Giladi_et_al")
beta_c$Area <- beta_c$Area/100

# Lizards
beta_A_f<-ggplot(data = beta_a, aes(x=Area, y=value, colour=index, fill=index,shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  scale_colour_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_S_n"=17, "beta_S_PIE" = 16),
                     labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                              "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+
  scale_y_continuous(trans = "log10",limits=c(0.65,14)) +
  
  
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
  
  scale_colour_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_S_n"=17, "beta_S_PIE" = 16),
                     labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                              "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  
  
  scale_x_continuous(trans = "log10",breaks = c(10, 100, 1000,10000))+  scale_y_continuous(trans = "log10",limits=c(0.95,30)) +
  
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
  
  scale_colour_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                      labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                               "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_fill_manual(name="",values = c("beta_S_n"="#91bfdb", "beta_S_PIE" = "#af8dc3"),
                    labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
                             "beta_S_PIE"=expression(paste(beta,"S"["PIE"]))))+
  scale_shape_manual(name="",values = c("beta_S_n"=17, "beta_S_PIE" = 16),
                     labels=c("beta_S_n"=expression(paste(beta,"S"["n"])),
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

isar_figure<-plot_grid(gamma_A_f,alpha_A_f,beta_A_f,
                    gamma_B_f,alpha_B_f,beta_B_f,
                    gamma_C_f,alpha_C_f,beta_C_f,
                    labels=c("a) Total","b) Local","c) Beta",
                             "d) Total","e) Local","f) Beta",
                             "g) Total","h) Local","i) Beta"),
                    label_size=11, label_fontface = "bold", 
                    hjust=-1.3, vjust=1, ncol=3, align="hv")

ggsave(filename = "isar_figure_final.png", plot = isar_figure, 
       device = "png",dpi=500, width = 25, height = 25, units = "cm")

isar_figure




