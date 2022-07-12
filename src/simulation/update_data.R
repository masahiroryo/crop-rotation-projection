library(ranger)
library(caret)
library(tidyverse)
library(data.table)
library(dtplyr)

update_data <- function(init_data, model) {
  
  updated_data <- init_data %>% 
    as.data.table()
  
  for (year in unique(updated_data$Year)) {
    print(year)
    dat <- updated_data %>% 
      filter(Year==year) %>% 
      as.data.table()
    
    .temp <- dat %>% 
      select(-c("OBJECTID","cell_id")) %>% 
      select(-"CType") %>%
      as.data.table()
    
    pred <- predict(model, data = .temp)
    
    dat$CType <- pred$predictions

    updated_data <- updated_data %>%
      mutate(CType = replace(CType, Year==year, dat$CType)) %>%
      as.data.table()

    if (year < unique(init_data$Year)[length(unique(init_data$Year))]) { # if not the last year
      prev_ctype <- updated_data %>%
        filter(Year==year) %>%
        as.data.table()

      updated_data <- updated_data %>%
        mutate(PCType = replace(PCType, Year==(year+1), prev_ctype$CType)) %>%
        mutate(PPCType = replace(PPCType, Year==(year+1), prev_ctype$PCType)) %>%
        as.data.table()
    }
  }
  return(updated_data)
}



