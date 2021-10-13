# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample 2111571 .csv", sep = ",", header = TRUE)
gc()

# analysis ------------------------------------------------------------------------------------

smaller <- data %>% 
  sample_n(50000) %>% 
  as.data.table()

ggplot(data = smaller) + 
  geom_point(mapping = aes(x = x_coord, y = y_coord, color = state), position = "jitter", stroke = 0.1)

ggplot(data) + 
  geom_bar(mapping = aes(x = state))
  

crop <- data %>% 
  select(cr)
ggplot(smaller) + 
  geom_bar(mapping = aes(x = crtype))
