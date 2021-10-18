# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
gc()

# analysis ------------------------------------------------------------------------------------

smaller <- data %>% 
  sample_n(50000) %>% 
  as.data.table()

ggplot(data = smaller) + 
  geom_point(mapping = aes(x = x_coord, y = y_coord), position = "jitter", stroke = 0.1)

ggplot(smaller) + 
  geom_bar(mapping = aes(x = crtype))

crop <- data %>% 
  select(crType)
