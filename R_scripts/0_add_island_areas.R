library(dplyr)

meta1 <- read.csv("ISAR_DATA/island_area_data.csv", stringsAsFactors = F)
meta1 <- meta1 %>% select(case_study_ID, Site = island_name, Area, Unit = unit..area.)

dim(meta1)
length(unique(meta1$Site))

ryberg1 <- read.csv("ISAR_DATA/datasets/grasshopper_glades_RybergChase.csv", stringsAsFactors = F)

surendran1 <- read.csv("ISAR_DATA/datasets/lizards_islands_SurendranVasudevan.csv", stringsAsFactors = F)

names(meta1)
names(ryberg1)

ryberg2 <- ryberg1 %>% 
  left_join(meta1) %>% 
  select(-case_study_ID) %>%
  select(Site, Area, Unit, everything())

write.csv(ryberg2, "ISAR_DATA/datasets/grasshopper_glades_RybergChase2.csv", row.names = F)

surendran2 <- surendran1 %>% 
  left_join(meta1) %>% 
  select(-case_study_ID) %>%
  select(Site, Area, Unit, everything())

write.csv(surendran2, "ISAR_DATA/datasets/lizards_islands_SurendranVasudevan2.csv", row.names = F)
