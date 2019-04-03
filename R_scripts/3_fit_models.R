library(tidyr)
library(purrr)
library(dplyr)
library(broom)

dat1 <- read.csv("ISAR_DATA/diversity_indices/allstudies_allscales_allindices.csv", stringsAsFactors = F)
names(dat1)

by_index <- dat1 %>%
  group_by(Study, Scale, index) %>%
  nest()

by_index2 <- by_index %>% 
  mutate(model   = map(data, ~ lm(log(value) ~ log(Area), data = .)),
         Intercept = map_dbl(model, ~coef(.x)[1]),
         Slope     = map_dbl(model, ~coef(.x)[2]),
         glance    = map(model, glance)
         ) %>% 
  unnest(glance)

names(by_index2)

by_index2a <- by_index2 %>%
  select(Study:r.squared, -model, -data, p.value)

write.csv(by_index2a, "ISAR_DATA/results/Table_2.csv", row.names = FALSE)

