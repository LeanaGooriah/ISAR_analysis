#GGplot of lizards
library(ggplot2)
library(readr)
setwd("~/Desktop/ISAR ggplot")

lizards_plot_data <- read.csv("lizards_ggplot.csv", encoding = "UTF-8")

lizards_plot_data$L_T_B_x = factor(lizards_plot_data$L_T_B, levels=c('A) Total','B) Local','C) Beta'))

lizards_plot_data$Metrics = factor(lizards_plot_data$Metrics,
                                     levels = c(levels(lizards_plot_data$Metrics)[7], levels(lizards_plot_data$Metrics)[1:6]),
                                     ordered = TRUE)


library(ggplot2)

lizards_data_all <- ggplot(data = lizards_plot_data) +
  geom_point(aes(
    x = island_area,
    y = Value,
    col = Metrics,
    shape = Metrics
  ),
  size = 2.0) +
  stat_smooth(
    aes(
      x = island_area,
      y = Value,
      col = Metrics,
      linetype = Metrics,
      fill = Metrics
    ),
    method = "lm",
    se = TRUE
  ) +
  theme_bw()+
  facet_grid(.~L_T_B_x) + theme(aspect.ratio = 1, legend.position = "bottom") +
  labs(x="Island Area (km2)", y="Species Number") +
  theme(strip.text.x = element_text(size = 11, colour = "Black", face = "bold"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.title=element_blank(), legend.position="none")+
  guides(color=guide_legend("Metrics"), shape =guide_legend("Metrics"), linetype=guide_legend("Metrics")) +
  scale_x_continuous(trans = "log10", breaks = c(1, 100, 1000)) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = c("#d73027","#5ab4ac","#d8b365","#7fbf7b","#fc8d59","#91bfdb","#af8dc3")) +
  scale_fill_manual(values = c("#d73027","#5ab4ac","#d8b365","#7fbf7b","#fc8d59","#91bfdb","#af8dc3")) + 
  scale_shape_manual(values = c(8,17,16,17,16,17,16)) 


  lizards_data_all 
  
###Plot only legend
  library(grid)
  library(gridExtra)
  # create inset table 
  my_table <- tableGrob(head(lizards_plot_data)[, 1:3]) 
  
  # extract Legend 
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
  legend <- g_legend(lizards_data_all) 
  
  # or use cowplot package
  # legend <- cowplot::get_legend(my_hist)
  
  grid.newpage()
  grid.draw(legend)
  legend
#scale_color_discrete(viridis::inferno(7))
#factor(levels = Metrics)

#lizards_data_all + scale_color_manual(values=wes_palette(n=7, name="Darjeeling1", type = "continuous"))

#ggsave(filename = "test.pdf", plot = lizards_data_all, device = "pdf", width = 18, height = 15, units = "cm")

#GLADES
glades_plot <- read_csv("glades_ggplot.csv")
glades_plot <- subset(glades_plot, Metrics != "S sample")


glades_plot$BTS_x = factor(glades_plot$BTS, levels=c('D) Total','E) Local','F) Beta'))

# glades_plot$Metrics = factor(glades_plot$Metrics,
                                   # levels = c(levels(glades_plot$Metrics)[7], levels(glades_plot$Metrics)[1:6]),
                                   # ordered = TRUE)
glades_all <- ggplot(data = glades_plot) +
  geom_point(aes(x = glade_area, y = Value, col = Metrics, shape = Metrics), size=2.0) +
  stat_smooth(aes(x = glade_area, y = Value, col = Metrics, linetype=Metrics, fill = Metrics),method="lm", se=TRUE) + 
  theme_bw() +
  facet_grid(.~BTS_x) + theme(aspect.ratio = 1,legend.position = "bottom") +
  labs(x="Glade Area (km2)", y="Species Number") +
  theme(strip.text.x = element_text(size = 11, colour = "Black", face = "bold"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.title=element_blank(),legend.position="none")+
  guides(color=guide_legend("Metrics"), shape =guide_legend("Metrics"), linetype=guide_legend("Metrics")) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = c("#7fbf7b","#fc8d59","#91bfdb","#af8dc3","#5ab4ac","#d8b365","#d73027")) +
  scale_fill_manual(values = c("#7fbf7b","#fc8d59","#91bfdb","#af8dc3","#5ab4ac","#d8b365","#d73027")) + 
  scale_shape_manual(values = c(17,16,17,16,17,16,8))
glades_all 


#c("green4","tan3","tomato","lightseagreen","deepskyblue","lightslateblue","maroon1")
#glades_all + scale_color_manual(values=wes_palette(n=7, name="Darjeeling1", type = "continuous"))



#library(colorspace)
#pal <- choose_palette()

#GILALDI 

setwd("~/Desktop/ISAR ggplot")

gilaldi_ggplot <- read_csv("gilaldi_ggplot.csv")

gilaldi_ggplot <- subset(gilaldi_ggplot, Site != "12")
gilaldi_ggplot$BTS_x = factor(gilaldi_ggplot$BTS, levels=c('G) Total','H) Local','I) Beta'))


gilaldi_all <- ggplot(data = gilaldi_ggplot) +
  geom_point(aes(x = area, y = Value, col = Metrics, shape = Metrics), size=2.0) +
  stat_smooth(aes(x = area, y = Value, col = Metrics, linetype=Metrics, fill = Metrics),method="lm", se=TRUE) + 
  theme_bw() +
  facet_grid(.~BTS_x) + theme(aspect.ratio = 1,legend.position = "bottom") +
  labs(x="Fragment Area (ha)", y="Species Number") +
  theme(strip.text.x = element_text(size = 11, colour = "Black", face = "bold"), legend.title=element_blank(),legend.position="none")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  guides(color=guide_legend("Metrics"), shape =guide_legend("Metrics"), linetype=guide_legend("Metrics")) +
  scale_x_continuous(trans = "log10", breaks = c(0.5, 1, 2, 3)) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = c("#7fbf7b","#fc8d59","#91bfdb","#af8dc3","#5ab4ac","#d8b365","#d73027")) +
  scale_fill_manual(values = c("#7fbf7b","#fc8d59","#91bfdb","#af8dc3","#5ab4ac","#d8b365","#d73027")) + 
  scale_shape_manual(values = c(17,16,17,16,17,16,8))
gilaldi_all 



library(ggpubr)
case_studies <- ggarrange(lizards_data_all, glades_all, gilaldi_all, ncol = 1, nrow = 3)

ggsave(filename = "case_studies.png", plot = case_studies, device = "png", width = 25, height = 25, units = "cm")




