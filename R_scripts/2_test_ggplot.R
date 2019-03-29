#######################
# make figures ########
# trial ###############
#######################
require(ggplot2)
require(dplyr)

############################
# Set path and directories #
############################

work_dir <- getwd()
data_path <- paste(work_dir,"/ISAR_DATA/ggplot_data",sep="")

# Read all data file names
filenames <- list.files(data_path, pattern="*.csv*", full.names = F)

# Make list of study ids
filename_roots <- gsub("_ggplot.csv","",filenames) # remove _data from filenames

study_ids <- unique(filename_roots)

###################
# 1.Read in data  #
###################

s<-length(unique(study_ids))
data_out<-list()

for (i in 1:s){
  
  data_file <- paste(data_path,"/", study_ids[i], "_ggplot.csv", sep ="")
  data_out[[i]] <- read.csv(data_file, header = TRUE)
}

datt_1<-data_out[[1]] # andamans
datt_1<-select(datt_1, Site=island_name, area=island_area)
datt_1<-distinct(datt_1)
datt_1$Study<-"lizards_islands_SurendranVasudevan"

datt_2<-data_out[[2]] # plant fragments
datt_2<-select(datt_2, Site, area)
datt_2<-distinct(datt_2)
datt_2$Site<-as.factor(datt_2$Site)
datt_2$Study<-"plants_habitatfragments_Giladi"

datt_3<-data_out[[3]] # glades
datt_3<-select(datt_3, Site=Glade, area=glade_area)
datt_3<-distinct(datt_3)
datt_3$Site<-substr(datt_3$Site,6,10)
datt_3$Study<-"grasshopper_glades_RybergChase"

area_study<-rbind.data.frame(datt_1,datt_2)
area_study<-rbind.data.frame(area_study,datt_3)

div_indices<-read.csv("ISAR_DATA/diversity_indices/allstudies_allscales_allindices.csv",header=T)

div_indices_area<-left_join(div_indices, area_study, by.y=c("Study","Site"))

#

andamans<-filter(div_indices_area, Study=="lizards_islands_SurendranVasudevan")

lizards<-ggplot(data = div_indices_area, aes(x=area, y=value, colour=index, shape=index)) +
  geom_point(size=2)+
  stat_smooth(method="lm",se=TRUE)+
  
  # scale_color_manual(values = c("#d73027","#5ab4ac","#d8b365","#7fbf7b","#fc8d59","#91bfdb","#af8dc3")) +
  # scale_fill_manual(values = c("#d73027","#5ab4ac","#d8b365","#7fbf7b","#fc8d59","#91bfdb","#af8dc3")) + 
  # scale_shape_manual(values = c(8,17,16,17,16,17,16)) 

  scale_x_continuous(trans = "log10")+ #, breaks = c(1, 100, 1000,10000)) +
  scale_y_continuous(trans = "log10") +
  facet_grid(Study~Scale,scales="free") + theme(aspect.ratio = 1, legend.position = "bottom") +

  labs(x="Island Area (km2)", y="Species Number") +
  guides(color=guide_legend("Metrics"), shape =guide_legend("Metrics"), linetype=guide_legend("Metrics"))+ 

  theme_bw()+theme(aspect.ratio = 1,
                   strip.text.x = element_text(size = 11, colour = "Black", face = "bold"),
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=14,face="bold"),
                   legend.title=element_blank(), 
                  legend.position="none")


